library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--samples-prior')
parser$add_argument('--output')
args <- parser$parse_args()

set.seed(20240526)

samples <- readRDS(args$samples_prior)
sample_id <- sample(samples$n_samples, 1)

output <- samples$alpha_df %>%
  select(-c(value, value_samples)) %>%
  mutate(
    value = samples$alpha[sample_id, ]
  )

fst::write_fst(output, args$output)
