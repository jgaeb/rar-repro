library(groundhog)
pkgs <- c(
  "argparse",
  "glmnet",
  "fs",
  "tidymodels",
  "tidyverse"
)
groundhog.library(pkgs, "2024-01-22")

#################################### SETUP #####################################
# Set the seed for replicability.
set.seed(708308962)

# Parse command line arguments.
parser <- ArgumentParser()
parser$add_argument("--n_workers", type = "integer")
parser$add_argument("--feat_set", type = "character")
args <- parser$parse_args()

# Parallelization for risk model.
n_workers <- args$n_workers

# Load in the raw data.
d <- read_rds(path("data", "sqf.rds")) %>%
  as_tibble() %>%
  # Fit on 2008 and 2009 from frisked individuals, and assess on remaining
  # data.
  mutate(
    split = if_else(
      year %in% c("2008", "2009"),
      "analysis",
      "assessment"
    ),
    found.weapon = as_factor(found.weapon),
    race = factor(suspect.race, c("Black", "Hispanic", "White"))
  ) %>%
  select(-suspect.race) %>%
  make_splits(
    x = with(., list(
      analysis = seq(nrow(.))[split == "analysis"],
      assessment = seq(nrow(.))[split == "assessment"]
    )),
    data = .
  )

################################# FEATURES #####################################
# There is no disparate treatment in the simulation, so we remove race, in
# addition to suspected crime and precinct relative to the base features.
sim_feats <- c(
    "month",
    "hour",
    "location.housing",
    "suspect.sex",
    "suspect.age",
    "suspect.height",
    "suspect.weight",
    "suspect.hair",
    "suspect.eye",
    "suspect.build",
    "additional.report",
    "additional.investigation",
    "additional.proximity",
    "additional.evasive",
    "additional.associating",
    "additional.direction",
    "additional.highcrime",
    "additional.time",
    "additional.sights",
    "additional.other",
    "stopped.bc.object",
    "stopped.bc.desc",
    "stopped.bc.casing",
    "stopped.bc.lookout",
    "stopped.bc.clothing",
    "stopped.bc.drugs",
    "stopped.bc.furtive",
    "stopped.bc.violent",
    "stopped.bc.bulge",
    "stopped.bc.other"
  )

fct_feats <- colnames(training(d))[map_lgl(training(d), is.factor)]

################################# SIMULATION ###################################
# Factors with small levels can cause problems, so we lump them.
d_sim_weap <- training(d) %>%
  mutate(across(any_of(fct_feats), fct_lump_min, min = 500)) %>%
  filter(frisked)

m_sim_weap <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(
    formula(glue::glue(
      "found.weapon ~ { str_c(sim_feats, collapse = ' + ') }"
    )),
    data = d_sim_weap
  )

d_sim_frisk <- training(d) %>%
  mutate(
    frisked = as_factor(frisked),
    across(any_of(fct_feats), fct_lump_min, min = 500)
  )

m_sim_frisk <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(
    formula(glue::glue(
      "frisked ~ { str_c(sim_feats, collapse = ' + ') }"
    )),
    data = d_sim_frisk
  )

d_sim <- testing(d) %>%
  mutate(
    # Relevel so that fitted model coefficients don't need to be modified.
    race = fct_relevel(race, "White"),
    # Remove small levels to prevent sampling errors
    across(any_of(fct_feats), fct_lump_min, min = 500)
  ) %>%
  select(race, all_of(sim_feats)) %>%
  drop_na()

# Define the simulation function
sim <- function(n = 1e6, m = 100) {
  # Noise the model coefficients
  m_frisk <- m_sim_frisk
  m_frisk$fit$coefficients <- m_frisk$fit$coefficients +
    rnorm(length(m_frisk$fit$coefficients), 0, 1/50)
  m_weap <- m_sim_weap
  m_weap$fit$coefficients <- m_weap$fit$coefficients +
    rnorm(length(m_weap$fit$coefficients), 0, 1/50)

  # Sample 1,000,000 rows (with replacement), apply the models, and realize the
  # outcomes.
  df <- slice_sample(d_sim, n = n, replace = TRUE) %>%
    mutate(
      epsilon = rnorm(n, 1/10),
      p_frisk = predict(m_frisk, new_data = ., type = "prob") %>%
        pull(.pred_TRUE),
      p_weap = predict(m_weap, new_data = ., type = "prob") %>%
        pull(.pred_TRUE),
      frisked = runif(n) < p_frisk,
      weapon = runif(n) < p_weap,
      frisked = factor(frisked, levels = c("TRUE", "FALSE")),
      weapon = factor(weapon, levels = c("TRUE", "FALSE"))
    )

  # Calculate the estimand.
  quants <- with(df, quantile(p_weap, seq(0, m) / m))
  estimand <- df %>%
    group_by(bin = cut(p_weap, quants, include.lowest = TRUE)) %>%
    summarize(
      diff_b = mean(frisked[race == "Black"] == TRUE) -
        mean(frisked[race == "White"] == TRUE),
      diff_h = mean(frisked[race == "Hispanic"] == TRUE) -
        mean(frisked[race == "White"] == TRUE),
      .groups = "drop"
    ) %>%
    summarize(
      race = c("Black", "Hispanic"),
      true = c(mean(diff_b), mean(diff_h))
    )

  # Split the data into training and test.
  train <- slice(df, seq(floor(n/2)))
  test <- slice(df, seq(floor(n/2)+1, n))

  # Train a risk model.
  m_sim_risk <- logistic_reg(penalty = 0.1) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(
    formula(glue::glue(
      "weapon ~ { str_c(sim_feats, collapse = ' + ') }"
    )),
    data = filter(train, frisked == TRUE)
  )

  # Apply predictions to the test data.
  test <- predict(m_sim_risk, test, type = "prob") %>%
    select(p_est = .pred_TRUE) %>%
    bind_cols(test)

  # Fit a risk-adjusted regression.
  lm(frisked ~ race + p_est, data = mutate(test, frisked = frisked == TRUE)) %>%
    broom::tidy() %>%
    filter(str_detect(term, "race")) %>%
    mutate(term = str_remove(term, "^race")) %>%
    rename(race = term) %>%
    select(race, estimate, std.error) %>%
    left_join(estimand, by = "race")
}

# Run 1000 iterations of the simulation; then, plot the results.
map_dfr(seq(100), ~sim()) %>%
  write_rds(path("data", "sim.rds"), compress = "gz")
