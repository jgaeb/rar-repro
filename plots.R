library(groundhog)
pkgs <- c(
  "fs",
  "glue",
  "scales",
  "broom",
  "tidymodels",
  "tidyverse"
)
groundhog.library(pkgs, "2024-01-22")

#################################### SETUP #####################################
# Set the seed for replicability.
set.seed(708308962)

# Document text width (in inches) for sizing plots..
textwidth <- 406.0 / 72.27
square_height <- (1/2) * textwidth
square_width <-  (1/2) * textwidth
rect_height <-   (1/2) * textwidth
rect_width <-    (3/4) * textwidth

# Set ggplot theme.
theme_set(theme_bw())
theme_update(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

################################# FEATURES #####################################
# Layout features for use in the disparity analysis.
feats_ks <- c(
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

##################################### DATA #####################################
# Load the data
d <- expand_grid(feat_set = c("base", "conf", "rb")) %>%
  mutate(
    data = map(
      feat_set,
      ~ read_rds(
        path("data", glue("risk_{.}.rds")),
      )
    ),
    data = map2(
      data,
      feat_set,
      ~ rename(.x, "risk_{.y}" := risk)
    ),
    data = map(
      data,
      ~ mutate(.x, i = row_number())
    ),
    data = map_at(
      data,
      2:3,
      ~ select(., starts_with("risk_"))
    )
  ) %>%
  pull(data) %>%
  bind_cols() %>%
  select(i, starts_with("risk_"), everything()) %>%
  mutate(found.weapon = fct_relevel(found.weapon, "TRUE"))

# Load the base and race-blind sensitivity data.
sens <- expand_grid(feat_set = c("base", "rb")) %>%
  mutate(
    data = map(
      feat_set,
      ~ read_csv(path("data", glue("sens_{.}.csv")), col_types = "d")
    )
  ) %>%
  unnest(data)
# Load the location-stratified sensitivity data.
sens_loc <- expand_grid(pct_nw = c("h", "m", "l"), feat_set = c("base", "rb")) %>%
  mutate(
    sens = map2(
      feat_set,
      pct_nw,
      ~ read_csv(path("data", glue("sens_{.x}_{.y}.csv")), col_types = "d"),
    )
  ) %>%
  unnest(sens)

# Load the simulation results
sim <- read_rds(path("data", "sim.rds"))

################################ MAIN PAPER ####################################

# Plot the risk distribution.
risk_avg <- d %>%
  group_by(race) %>%
  summarize(
    risk_base = mean(risk_base, na.rm = TRUE),
    risk_rb   = mean(risk_rb, na.rm = TRUE)
  )

p_risk_base <- d %>%
  ggplot(aes(x = risk_base, color = race, linetype = race)) +
  geom_density() +
  scale_x_continuous(
    "Estimated risk",
    limits = c(0, 0.1),
    labels = label_percent(0.1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    "Density",
    expand = c(0, 1),
    breaks = NULL,
    labels = NULL
  ) +
  geom_vline(
    aes(xintercept = risk_base, color = race),
    data = risk_avg,
    linetype = "dotted"
  ) +
  scale_color_viridis_d() +
  labs(color = "Race", linetype = "Race") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

p_risk_rb <- d %>%
  ggplot(aes(x = risk_rb, color = race, linetype = race)) +
  geom_density() +
  scale_x_continuous(
    "Estimated risk",
    limits = c(0, 0.1),
    labels = label_percent(0.1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    "Density",
    expand = c(0, 1),
    breaks = NULL,
    labels = NULL
  ) +
  geom_vline(
    aes(xintercept = risk_rb, color = race),
    data = risk_avg,
    linetype = "dotted"
  ) +
  scale_color_viridis_d() +
  labs(color = "Race", linetype = "Race") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

ggsave(
  path("figures", "risk_base.pdf"),
  plot = p_risk_base,
  width = rect_width,
  height = rect_height,
  device = cairo_pdf
)

ggsave(
  path("figures", "risk_rb.pdf"),
  plot = p_risk_base,
  width = rect_width,
  height = rect_height,
  device = cairo_pdf
)

# Plot the risk distribution faceted by outcome.
p_risk_by_outcome_base <- d %>%
  mutate(
    found.weapon = if_else(
      found.weapon == TRUE,
      "Found weapon",
      "No weapon"
    )
  ) %>%
  filter(frisked) %>%
  ggplot(aes(
    x = risk_base,
    y = after_stat(density),
    color = race,
    linetype = race
  )) +
  geom_density() +
  scale_x_continuous(
    "Estimated risk",
    limits = c(0, 0.1),
    labels = label_percent(0.1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    "Density",
    expand = c(0, 1),
    breaks = NULL,
    labels = NULL
  ) +
  scale_color_viridis_d() +
  labs(color = "Race", linetype = "Race") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  ) +
  facet_grid(rows = vars(found.weapon))

p_risk_by_outcome_rb <- d %>%
  mutate(
    found.weapon = if_else(
      found.weapon == TRUE,
      "Found weapon",
      "No weapon"
    )
  ) %>%
  filter(frisked) %>%
  ggplot(aes(
    x = risk_rb,
    y = after_stat(density),
    color = race,
    linetype = race
  )) +
  geom_density() +
  scale_x_continuous(
    "Estimated risk",
    limits = c(0, 0.1),
    labels = label_percent(0.1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    "Density",
    expand = c(0, 1),
    breaks = NULL,
    labels = NULL
  ) +
  scale_color_viridis_d() +
  labs(color = "Race", linetype = "Race") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  ) +
  facet_grid(rows = vars(found.weapon))

ggsave(
  path("figures", "risk_by_outcome_base.pdf"),
  plot = p_risk_by_outcome_base,
  width = rect_width,
  height = rect_width,
  device = cairo_pdf
)

ggsave(
  path("figures", "risk_by_outcome_rb.pdf"),
  plot = p_risk_by_outcome_rb,
  width = rect_width,
  height = rect_width,
  device = cairo_pdf
)

# Plot the frisk policy.
p_policy_base <- d %>%
  ggplot(aes(
    x = risk_base,
    y = as.double(frisked),
    color = race,
    linetype = race
  )) +
  geom_smooth(
    formula = y ~ x,
    method = "glm",
    method.args = list(family = binomial)
  ) +
  scale_x_continuous(
    "Estimated risk",
    limits = c(0, 0.1),
    labels = label_percent(0.1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    "Probability of frisk",
    limits = c(0, 1),
    labels = label_percent(1),
    expand = c(0, 0)
  ) +
  scale_color_viridis_d() +
  labs(color = "Race", linetype = "Race") +
  theme(
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.background = element_blank()
  )

p_policy_rb <- d %>%
  ggplot(aes(
    x = risk_rb,
    y = as.double(frisked),
    color = race,
    linetype = race
  )) +
  geom_smooth(
    formula = y ~ x,
    method = "glm",
    method.args = list(family = binomial)
  ) +
  scale_x_continuous(
    "Estimated risk",
    limits = c(0, 0.1),
    labels = label_percent(0.1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    "Probability of frisk",
    limits = c(0, 1),
    labels = label_percent(1),
    expand = c(0, 0)
  ) +
  scale_color_viridis_d() +
  labs(color = "Race", linetype = "Race") +
  theme(
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.background = element_blank()
  )

ggsave(
  path("figures", "policy_base.pdf"),
  plot = p_policy_base,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

ggsave(
  path("figures", "policy_rb.pdf"),
  plot = p_policy_rb,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

# Measure the raw disparities in the data.
di_raw <- d %>%
  mutate(race = fct_relevel(race, "White")) %>%
  lm(frisked ~ race, data = .) %>%
  tidy() %>%
  filter(str_detect(term, "race")) %>%
  mutate(
    term = str_extract(term, "(?<=race).+$"),
    type = "Raw disparity"
  )

# Fit a linear model to the policy.
di_ks <- read_rds(path("data", "sqf.rds")) %>%
  as_tibble() %>%
  filter(year %in% c(2010, 2011)) %>%
  mutate(race = fct_relevel(suspect.race, "White")) %>%
  lm(
    formula(glue::glue(
      "frisked ~ { str_c(feats_ks, collapse = ' + ') }"
    )),
    data = .,
  ) %>%
  tidy() %>%
  filter(str_detect(term, "race")) %>%
  mutate(
    term = str_extract(term, "(?<=race).+$"),
    type = "Kitchen sink"
  )

# Fit a linear risk-adjusted regression.
di_rar_base <- d %>%
  mutate(race = fct_relevel(race, "White")) %>%
  lm(frisked ~ race + risk_base, data = .) %>%
  tidy() %>%
  filter(str_detect(term, "race")) %>%
  mutate(
    term = str_extract(term, "(?<=race).+$"),
    type = "Risk-adjusted"
  )

di_rar_rb <- d %>%
  mutate(race = fct_relevel(race, "White")) %>%
  lm(frisked ~ race + risk_rb, data = .) %>%
  tidy() %>%
  filter(str_detect(term, "race")) %>%
  mutate(
    term = str_extract(term, "(?<=race).+$"),
    type = "Risk-adjusted"
  )

# Plot the results, faceting by the method.
# NOTE: The error bars are too small to see.
p_comparison_base <- bind_rows(di_raw, di_ks, di_rar_base) %>%
  mutate(type = factor(
    type,
    levels = c("Raw disparity", "Kitchen sink", "Risk-adjusted")
  )) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  xlab(NULL) +
  scale_y_continuous(
    "Estimated disparities",
    limits = c(0, 1/5),
    expand = c(0, 0),
    labels = label_percent(1)
  ) +
  facet_wrap("type")

p_comparison_rb <- bind_rows(di_raw, di_ks, di_rar_rb) %>%
  mutate(type = factor(
    type,
    levels = c("Raw disparity", "Kitchen sink", "Risk-adjusted")
  )) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  xlab(NULL) +
  scale_y_continuous(
    "Estimated disparities",
    limits = c(0, 1/5),
    expand = c(0, 0),
    labels = label_percent(1)
  ) +
  facet_wrap("type")

ggsave(
  path("figures", "comparison_base.pdf"),
  plot = p_comparison_base,
  width = rect_width,
  height = rect_height,
  device = cairo_pdf
)

ggsave(
  path("figures", "comparison_rb.pdf"),
  plot = p_comparison_rb,
  width = rect_width,
  height = rect_height,
  device = cairo_pdf
)

# Plot the sensitivity analysis.
p_sensitivity_base <- sens %>%
  filter(feat_set == "base") %>%
  pivot_longer(
    cols      = !c(feat_set, epsilon),
    names_to  = "var",
    values_to = "val"
  ) %>%
  mutate(
    race = case_when(
      str_detect(var, "Black") ~ "Black",
      str_detect(var, "Hispanic") ~ "Hispanic"
    ),
    var  = str_remove(var, "_(Black|Hispanic)")
  ) %>%
  pivot_wider(
    names_from = var,
    values_from = val
  ) %>%
  ggplot(aes(x = epsilon / with(d, mean(abs(risk_base))))) +
  scale_x_continuous(
    bquote(paste(epsilon, " (as % of base rate)")),
    limits = c(0, 0.5),
    labels = label_percent()
  ) +
  scale_y_continuous(
    "Estimated disparities (p.p.)",
    limits = c(0, 0.75),
    labels = label_percent(1)
  ) +
  geom_ribbon(aes(ymin = beta_min,      ymax = beta_max), fill = "light blue") +
  geom_ribbon(aes(ymin = beta_min_02.5, ymax = beta_min_97.5), alpha = 1/2) +
  geom_ribbon(aes(ymin = beta_max_02.5, ymax = beta_max_97.5), alpha = 1/2) +
  geom_vline(
    xintercept = with(d, mean(abs(risk_base - risk_conf)) / mean(risk_base)),
    linetype = "dotted"
  ) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(race)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_sensitivity_rb <- sens %>%
  filter(feat_set == "rb") %>%
  pivot_longer(
    cols      = !c(feat_set, epsilon),
    names_to  = "var",
    values_to = "val"
  ) %>%
  mutate(
    race = case_when(
      str_detect(var, "Black") ~ "Black",
      str_detect(var, "Hispanic") ~ "Hispanic"
    ),
    var  = str_remove(var, "_(Black|Hispanic)")
  ) %>%
  pivot_wider(
    names_from = var,
    values_from = val
  ) %>%
  ggplot(aes(x = epsilon / with(d, mean(abs(risk_base))))) +
  scale_x_continuous(
    bquote(paste(epsilon, " (as % of base rate)")),
    limits = c(0, 0.5),
    labels = label_percent()
  ) +
  scale_y_continuous(
    "Estimated disparities (p.p.)",
    limits = c(0, 0.75),
    labels = label_percent(1)
  ) +
  geom_ribbon(aes(ymin = beta_min,      ymax = beta_max), fill = "light blue") +
  geom_ribbon(aes(ymin = beta_min_02.5, ymax = beta_min_97.5), alpha = 1/2) +
  geom_ribbon(aes(ymin = beta_max_02.5, ymax = beta_max_97.5), alpha = 1/2) +
  geom_vline(
    xintercept = with(d, mean(abs(risk_base - risk_conf)) / mean(risk_base)),
    linetype = "dotted"
  ) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(race)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  path("figures", "sensitivity_base.pdf"),
  plot = p_sensitivity_base,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

ggsave(
  path("figures", "sensitivity_rb.pdf"),
  plot = p_sensitivity_rb,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

# Plot the secret weapon
fixed_epsilon <- with(d, round(mean(abs(risk_base - risk_conf)), 4))
p_secret_weapon_base <- sens_loc %>%
  filter(feat_set == "base") %>%
  pivot_longer(
    cols      = !c(pct_nw, feat_set, epsilon),
    names_to  = "var",
    values_to = "val"
  ) %>%
  mutate(
    race = case_when(
      str_detect(var, "Black") ~ "Black",
      str_detect(var, "Hispanic") ~ "Hispanic"
    ),
    var  = str_remove(var, "_(Black|Hispanic)")
  ) %>%
  pivot_wider(
    names_from = var,
    values_from = val
  ) %>%
  group_by(pct_nw, race) %>%
  summarize(
    est = beta_min[epsilon == 0],
    beta_min = beta_min[epsilon == fixed_epsilon],
    beta_max = beta_max[epsilon == fixed_epsilon],
    beta_min_02.5 = beta_min_02.5[epsilon == fixed_epsilon],
    beta_min_97.5 = beta_min_97.5[epsilon == fixed_epsilon],
    beta_max_02.5 = beta_max_02.5[epsilon == fixed_epsilon],
    beta_max_97.5 = beta_max_97.5[epsilon == fixed_epsilon],
    .groups = "drop"
  ) %>%
  mutate(
    pct_nw = factor(pct_nw, levels = c("l", "m", "h")),
    pct_nw = fct_recode(pct_nw, "<83" = "l", "83\u201397" = "m", "97\u2013100" = "h")
  ) %>%
  ggplot(aes(x = pct_nw)) +
  geom_linerange(aes(y = est, ymin = beta_min, ymax = beta_max), linewidth = 8) +
  geom_errorbar(
    aes(y = beta_min, ymin = beta_min_02.5, ymax = beta_min_97.5),
    width = 0.2
  ) +
  geom_errorbar(
    aes(y = beta_max, ymin = beta_max_02.5, ymax = beta_max_97.5),
    width = 0.2
  ) +
  geom_point(aes(y = est), color = "white") +
  xlab("Stops of non-white individuals (%)") +
  scale_y_continuous(
    "Estimated disparities (p.p.)",
    labels = label_percent(1)
  ) +
  geom_hline(yintercept = 0) +
  facet_wrap("race") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(r = 0.51, unit = "cm")
  )

p_secret_weapon_rb <- sens_loc %>%
  filter(feat_set == "rb") %>%
  pivot_longer(
    cols      = !c(pct_nw, feat_set, epsilon),
    names_to  = "var",
    values_to = "val"
  ) %>%
  mutate(
    race = case_when(
      str_detect(var, "Black") ~ "Black",
      str_detect(var, "Hispanic") ~ "Hispanic"
    ),
    var  = str_remove(var, "_(Black|Hispanic)")
  ) %>%
  pivot_wider(
    names_from = var,
    values_from = val
  ) %>%
  group_by(pct_nw, race) %>%
  summarize(
    est = beta_min[epsilon == 0],
    beta_min = beta_min[epsilon == fixed_epsilon],
    beta_max = beta_max[epsilon == fixed_epsilon],
    beta_min_02.5 = beta_min_02.5[epsilon == fixed_epsilon],
    beta_min_97.5 = beta_min_97.5[epsilon == fixed_epsilon],
    beta_max_02.5 = beta_max_02.5[epsilon == fixed_epsilon],
    beta_max_97.5 = beta_max_97.5[epsilon == fixed_epsilon],
    .groups = "drop"
  ) %>%
  mutate(
    pct_nw = factor(pct_nw, levels = c("l", "m", "h")),
    pct_nw = fct_recode(pct_nw, "<83" = "l", "83\u201397" = "m", "97\u2013100" = "h")
  ) %>%
  ggplot(aes(x = pct_nw)) +
  geom_linerange(aes(y = est, ymin = beta_min, ymax = beta_max), linewidth = 8) +
  geom_errorbar(
    aes(y = beta_min, ymin = beta_min_02.5, ymax = beta_min_97.5),
    width = 0.2
  ) +
  geom_errorbar(
    aes(y = beta_max, ymin = beta_max_02.5, ymax = beta_max_97.5),
    width = 0.2
  ) +
  geom_point(aes(y = est), color = "white") +
  xlab("Stops of non-white individuals (%)") +
  scale_y_continuous(
    "Estimated disparities",
    labels = label_percent(1)
  ) +
  geom_hline(yintercept = 0) +
  facet_wrap("race") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(r = 0.51, unit = "cm")
  )

ggsave(
  path("figures", "secret_weapon_base.pdf"),
  plot = p_secret_weapon_base,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

ggsave(
  path("figures", "secret_weapon_rb.pdf"),
  plot = p_secret_weapon_base,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

################################ MODEL CHECKS ##################################

# Calculate risk model AUC.
p_roc_base <- d %>%
  filter(frisked) %>%
  roc_curve(found.weapon, risk_base) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  xlab("True positive rate") +
  ylab("False positive rate") +
  geom_abline(linetype = "dashed") +
  coord_equal()

p_roc_rb <- d %>%
  filter(frisked) %>%
  roc_curve(found.weapon, risk_rb) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  xlab("True positive rate") +
  ylab("False positive rate") +
  geom_abline(linetype = "dashed") +
  coord_equal()

ggsave(
  path("figures", "roc_curve_base.pdf"),
  plot = p_roc_base,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

ggsave(
  path("figures", "roc_curve_rb.pdf"),
  plot = p_roc_rb,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

d %>%
  filter(frisked == "TRUE") %>%
  roc_auc(found.weapon, risk_base) %>%
  glue::glue_data("Base risk model AUC on test data: { .estimate }")

d %>%
  filter(frisked == "TRUE") %>%
  roc_auc(found.weapon, risk_rb) %>%
  glue::glue_data("Race-blind risk model AUC on test data: { .estimate }")

# Show calibration on age, race, and gender
d_cal_age_base <- d %>%
  filter(frisked) %>%
  mutate(
    age = case_when(
      age >  40 ~ "Over 40",
      age >= 33 ~ "33 to 40",
      age >= 26 ~ "26 to 32",
      age >= 18 ~ "18 to 25",
      TRUE      ~ "Under 18"
    ),
    age = factor(
      age,
      levels = c("Under 18", "18 to 25", "26 to 32", "33 to 40", "Over 40")
    )
  ) %>%
  group_by(age) %>%
  summarize(
    Model = mean(risk_base),
    Empirical = mean(found.weapon == TRUE),
  ) %>%
  ungroup() %>%
  rename(category = age) %>%
  mutate(facet = "Age")

d_cal_age_rb <- d %>%
  filter(frisked) %>%
  mutate(
    age = case_when(
      age >  40 ~ "Over 40",
      age >= 33 ~ "33 to 40",
      age >= 26 ~ "26 to 32",
      age >= 18 ~ "18 to 25",
      TRUE      ~ "Under 18"
    ),
    age = factor(
      age,
      levels = c("Under 18", "18 to 25", "26 to 32", "33 to 40", "Over 40")
    )
  ) %>%
  group_by(age) %>%
  summarize(
    Model = mean(risk_rb),
    Empirical = mean(found.weapon == TRUE),
  ) %>%
  ungroup() %>%
  rename(category = age) %>%
  mutate(facet = "Age")

d_cal_race_base <- d %>%
  filter(frisked) %>%
  group_by(race) %>%
  summarize(
    Model = mean(risk_base),
    Empirical = mean(found.weapon == TRUE),
  ) %>%
  ungroup() %>%
  rename(category = race) %>%
  mutate(facet = "Race")

d_cal_race_rb <- d %>%
  filter(frisked) %>%
  group_by(race) %>%
  summarize(
    Model = mean(risk_rb),
    Empirical = mean(found.weapon == TRUE),
  ) %>%
  ungroup() %>%
  rename(category = race) %>%
  mutate(facet = "Race")

d_cal_gender_base <- d %>%
  filter(frisked) %>%
  mutate(gender = str_to_title(gender)) %>%
  group_by(gender) %>%
  summarize(
    Model = mean(risk_base),
    Empirical = mean(found.weapon == TRUE),
  ) %>%
  ungroup() %>%
  rename(category = gender) %>%
  mutate(facet = "Gender")

d_cal_gender_rb <- d %>%
  filter(frisked) %>%
  mutate(gender = str_to_title(gender)) %>%
  group_by(gender) %>%
  summarize(
    Model = mean(risk_rb),
    Empirical = mean(found.weapon == TRUE),
  ) %>%
  ungroup() %>%
  rename(category = gender) %>%
  mutate(facet = "Gender")

d_cal_base <- bind_rows(d_cal_age_base, d_cal_race_base, d_cal_gender_base) %>%
  pivot_longer(
    cols = c(Model, Empirical),
    names_to = "method",
    values_to = "measurement"
  )

d_cal_rb <- bind_rows(d_cal_age_rb, d_cal_race_rb, d_cal_gender_rb) %>%
  pivot_longer(
    cols = c(Model, Empirical),
    names_to = "method",
    values_to = "measurement"
  )

# Plot demographic calibration
p_cal_demo_base <- d_cal_base %>%
  ggplot(aes(y = category, x = measurement, fill = method)) +
  geom_point(shape = 21) +
  scale_x_continuous(labels = label_percent(0.1)) +
  scale_fill_manual(
    values = c(Model = "black", Empirical = "white"),
    name = NULL
  ) +
  facet_grid(rows = vars(facet), scales = "free_y") +
  labs(x = "Hit rate", y = NULL, method = NULL) +
  theme(
    legend.position = c(1, 1/2),
    legend.justification = c(1, 1/2),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

p_cal_demo_rb <- d_cal_rb %>%
  ggplot(aes(y = category, x = measurement, fill = method)) +
  geom_point(shape = 21) +
  scale_x_continuous(labels = label_percent(0.1)) +
  scale_fill_manual(
    values = c(Model = "black", Empirical = "white"),
    name = NULL
  ) +
  facet_grid(rows = vars(facet), scales = "free_y") +
  labs(x = "Hit rate", y = NULL, method = NULL) +
  theme(
    legend.position = c(1, 1/2),
    legend.justification = c(1, 1/2),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

ggsave(
  path("figures", "cal_demo_base.pdf"),
  plot = p_cal_demo_base,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

ggsave(
  path("figures", "cal_demo_rb.pdf"),
  plot = p_cal_demo_rb,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

# Plot location calibration
p_cal_loc_base <- d %>%
  mutate(
    location.housing = fct_relevel(
      location.housing,
      "housing",
      "transit",
      "neither"
    )
  ) %>%
  filter(frisked) %>%
  group_by(precinct, location.housing) %>%
  summarize(
    n = n(),
    Model = mean(risk_base),
    Empirical = mean(found.weapon == TRUE),
    .groups = "drop"
  ) %>%
  filter(n > 100) %>%
  ggplot(aes(x = Model, y = Empirical, color = location.housing, size = n)) +
  scale_size_area(guide = NULL) +
  scale_x_log10(labels = label_percent(0.1), limits = c(0.0015, 0.15)) +
  scale_y_log10(labels = label_percent(0.1), limits = c(0.0015, 0.15)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Model hit rate",
    y = "Empirical hit rate",
    color = "Location"
  ) +
  theme(
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.height = unit(0.1, "cm")
  )

p_cal_loc_rb <- d %>%
  mutate(
    location.housing = fct_relevel(
      location.housing,
      "housing",
      "transit",
      "neither"
    )
  ) %>%
  filter(frisked) %>%
  group_by(precinct, location.housing) %>%
  summarize(
    n = n(),
    Model = mean(risk_rb),
    Empirical = mean(found.weapon == TRUE),
    .groups = "drop"
  ) %>%
  filter(n > 100) %>%
  ggplot(aes(x = Model, y = Empirical, color = location.housing, size = n)) +
  scale_size_area(guide = NULL) +
  scale_x_log10(labels = label_percent(0.1), limits = c(0.0015, 0.15)) +
  scale_y_log10(labels = label_percent(0.1), limits = c(0.0015, 0.15)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Model hit rate",
    y = "Empirical hit rate",
    color = "Location"
  ) +
  theme(
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.height = unit(0.1, "cm")
  )

ggsave(
  path("figures", "cal_location_base.pdf"),
  plot = p_cal_loc_base,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

ggsave(
  path("figures", "cal_location_rb.pdf"),
  plot = p_cal_loc_rb,
  width = square_width,
  height = square_height,
  device = cairo_pdf
)

################################# SIMULATION ###################################

# Plot the results of the simulation
p_sim <- sim %>%
  ggplot(aes(x = estimate, y = true)) +
  geom_point() +
  geom_abline() +
  scale_x_continuous(labels = label_percent(1)) +
  scale_y_continuous(labels = label_percent(1)) +
  facet_wrap(vars(race)) +
  xlab("Estimated disparity") +
  ylab("True disparity") +
  coord_obs_pred()

ggsave(
  path("figures", "simulation.pdf"),
  plot = p_sim,
  width = rect_width,
  height = rect_height,
  device = cairo_pdf
)
