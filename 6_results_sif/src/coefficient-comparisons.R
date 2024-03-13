# source(Sys.getenv('UTILS_PARTIAL'))
# source(Sys.getenv('DISPLAY_PARTIAL'))

source('partials/utils.R')
source('partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(ggrepel)
library(patchwork)

# parser <- ArgumentParser()
# parser$add_argument('--samples')
# parser$add_argument('--output-base')
# args <- parser$parse_args()

args <- list(
  samples_base = '4_inversion/intermediates/samples-',
  output = '6_results_sif/figures/coefficient-comparisons.pdf'
)
N_MCMC_WARM_UP <- 100
N_MCMC_SAMPLES <- 200

samples_paths <- list.files(
  dirname(args$samples_base),
  pattern = basename(args$samples_base),
  full.names = TRUE
)
samples_paths_clean <- samples_paths[!grepl("\\d", basename(samples_paths))]


df_samples <- bind_rows(lapply(samples_paths_clean, function(path) {
  samples <- readRDS(path)
  alpha_df <- samples$alpha_df
  if (dim(samples$alpha)[1] > N_MCMC_SAMPLES) {
    alpha_df$value_samples <- t(samples$alpha[(N_MCMC_WARM_UP + 1):N_MCMC_SAMPLES, ])
  }

  sample_type <- sub('.*-(.*)\\..*', '\\1', basename(path))
  alpha_df %>%
    filter(component != 'residual') %>%
    mutate(type = sample_type) %>%
    select(-month)
})) %>%
  mutate(basis_vector_str = paste0(component, '.', region)) %>%
  arrange(type, component, region)

p <- ggplot(data = df_samples) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey50') +
  geom_point(aes(x = value, y = basis_vector_str, colour = type), shape = 1) +
  scale_y_discrete(limits = rev(levels(factor(df_samples$basis_vector_str)))) +
  facet_wrap(~inventory, scales = 'free_y', nrow = 2) +
  scale_colour_brewer(palette = 'Dark2') +
  labs(
    x = sprintf('Mean of Posterior Samples %d:%d', N_MCMC_WARM_UP + 1, N_MCMC_SAMPLES),
    y = NULL,
    colour = 'Obs. Groups'
  ) +
  theme(axis.text.y = element_text(size = 6))

ggsave_base(args$output, p, width = 20, height = 100)
