library(groundhog)
pkgs <- c(
  "xgboost",
  "argparse",
  "fs",
  "tidymodels",
  "glue",
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

# Which feature set to use.
feat_set <- args$feat_set

################################# FEATURES #####################################
# Layout base features for use in the model.
feats_base <- c(
    "race",
    "suspected.crime",
    "month",
    "hour",
    "precinct",
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

feats_list <- list(
  "base" = feats_base,
  "conf" = str_subset(feats_base, "^stopped.bc", negate = TRUE),
  "rb"   = str_subset(feats_base, "^race$", negate = TRUE)
)

feats <- feats_list[[feat_set]]

################################## PRECINCTS ##################################
# Split up precincts by the percentage of nonwhite residents.
precincts <- read_rds(path("data", "sqf.rds")) %>%
  as_tibble() %>%
  group_by(precinct) %>%
  summarize(pct_nw = mean(suspect.race != "White")) %>%
  mutate(
    pct_nw = case_when(
      pct_nw > round(quantile(pct_nw, 2/3), 2) ~ "h",
      pct_nw > round(quantile(pct_nw, 1/3), 2) ~ "m",
      TRUE                                     ~ "l"
    ),
    pct_nw = factor(pct_nw, c("h", "m", "l"))
  )

############################### RISK PREDICTION ###############################
# Load the raw data.
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
    race = factor(suspect.race, c("White", "Black", "Hispanic")),
    i = row_number()
  ) %>%
  select(-suspect.race) %>%
  make_splits(
    x = with(., list(
      analysis = seq(nrow(.))[split == "analysis"],
      assessment = seq(nrow(.))[split == "assessment"]
    )),
    data = .
  )

# Fit a GBM to predict risk.
# NOTE: We stop early if the model fails to improve for twenty iterations, as
#       calculated on an (internal) 10% validation set.
m_risk <- boost_tree(
    trees = 10000,
    tree_depth = 2,
    learn_rate = 0.1,
    stop_iter = 20
  ) %>%
  set_engine("xgboost", nthread = n_workers, validation = 0.1) %>%
  set_mode("classification") %>%
  fit(
    formula(glue::glue(
      "found.weapon ~ { str_c(feats, collapse = ' + ') }"
    )),
    data = filter(training(d), frisked)
  )

# Add predictions to the test data.
predict(m_risk, new_data = testing(d), type = "prob") %>%
  bind_cols(testing(d)) %>%
  select(
    i,
    found.weapon,
    frisked,
    race,
    age = suspect.age,
    gender = suspect.sex,
    risk = .pred_TRUE,
    precinct,
    location.housing
  ) %>%
  drop_na() %>%
  left_join(precincts, by = "precinct") %>%
  write_rds(path("data", glue("risk_{feat_set}.rds")), compress = "gz")
