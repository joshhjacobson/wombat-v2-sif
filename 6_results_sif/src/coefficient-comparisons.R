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

df_net <- df_samples %>% 
  select(-c(value_samples, basis_vector, basis_vector_str)) %>%
  filter(type %in% c('LNLGIS', 'LNLGISSIF')) %>%
  tidyr::pivot_wider(names_from = inventory, values_from = value) %>%
  tidyr::drop_na() %>%
  mutate(
    net_bio = bio_resp_tot + bio_assim,
    basis_vector_str = paste0(component, '.', region)
  ) %>%
  arrange(type, component, region)

p1 <- ggplot(data = df_samples %>% filter(type %in% c('LNLGIS', 'LNLGISSIF'))) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey50') +
  geom_point(aes(x = value, y = basis_vector_str, colour = type, shape = type)) +
  scale_y_discrete(limits = rev(levels(factor(df_samples$basis_vector_str)))) +
  facet_wrap(~inventory, scales = 'free_y', nrow = 2) +
  scale_colour_brewer(name = 'Obs. Groups', palette = 'Dark2') +
  scale_shape_manual(name = 'Obs. Groups', values = c(0, 1, 2, 5, 3, 4)) +
  labs(
    x = sprintf('Mean of Posterior Samples %d:%d', N_MCMC_WARM_UP + 1, N_MCMC_SAMPLES),
    y = NULL
  ) +
  theme(axis.text.y = element_text(size = 6))

p2 <- ggplot(data = df_net) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey50') +
  geom_point(aes(x = net_bio, y = basis_vector_str, colour = type, shape = type)) +
  scale_y_discrete(limits = rev(levels(factor(df_net$basis_vector_str)))) +
  scale_colour_brewer(name = 'Obs. Groups', palette = 'Dark2') +
  scale_shape_manual(name = 'Obs. Groups', values = c(0, 1, 2, 5, 3, 4)) +
  labs(
    x = 'Sum of coefficient posterior means (GPP + Respiration)',
    y = NULL
  ) +
  theme(axis.text.y = element_text(size = 6))

p <- p1 / p2

# ggsave_base(args$output, p, width = 20, height = 100)
ggsave_base(args$output, p, width = 20, height = 150, limitsize = FALSE)
