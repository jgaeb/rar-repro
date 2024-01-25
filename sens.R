library(groundhog)
pkgs <- c(
  "argparse",
  "glue",
  "rar",
  "fs",
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
parser$add_argument("--pct_nw", type = "character")
args <- parser$parse_args()

# Parallelization for risk model.
n_workers <- args$n_workers

# Which feature set to use.
feat_set <- args$feat_set

# Proportion non-white to use.
pct_nw <- args$pct_nw

#################################### DATA ######################################
# Load the risk data.
df <- read_rds(path("data", glue("risk_{feat_set}.rds")))

if (!is.null(pct_nw)) {
  # Filter to the desired proportion non-white.
  df <- df %>%
    filter(pct_nw == {{ pct_nw }})
}

############################ SENSITIVITY ANALYSIS ##############################

# Perform the sensitivity analysis.
sens(df, race, frisked, risk, "White", epsilon = 0.01, eta = 0.0005,
     n_threads = n_workers, N = 1000) %>%
  write_csv(path(
    "data",
    glue("sens_{feat_set}{if_else(!is.null(pct_nw), str_c('_', pct_nw), '')}.csv")
  ))
