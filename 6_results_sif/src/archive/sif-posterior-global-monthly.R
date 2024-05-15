library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(fst)
library(Matrix)
library(parallel)
library(patchwork)

# source(Sys.getenv('UTILS_PARTIAL'))
# source(Sys.getenv('DISPLAY_PARTIAL'))
source("partials/display.R")
source("partials/utils.R")

args <- list()
args$inventory <- '3_sif/intermediates/sib4-monthly-sif-2x25.nc'
args$model <- '3_sif/intermediates/model-sif-assim.fst'
args$perturbations <- '5_results/intermediates/perturbations-augmented.fst'
args$samples <- '4_inversion/intermediates/samples-free-resp-LNLGISSIF.rds'


model <- read_fst(args$model)
perturbations <- fst::read_fst(args$perturbations)
samples <- readRDS(args$samples)

perturbations_base <- perturbations %>%
  filter(
    inventory == 'bio_assim',
    time >= '2015-01-01',
    time < '2021-01-01'
  ) %>%
  mutate(
    # NOTE: sign of perturbations are switched to match SIF (yes confirmed, GPP is damped, thus so is SIF)
    value = -value,
    year_month = factor(time),
    month = lubridate::month(time),
  ) %>%
  inner_join(
    model %>% select(c(
      longitude = model_longitude,
      latitude = model_latitude,
      month,
      slope
    )),
    by = c('longitude', 'latitude', 'month')
  ) %>%
  mutate(
    value = value * slope
  )

perturbations_sif <- perturbations_base %>%
  group_by(year_month, basis_vector) %>%
  summarise(
    value = sum(area * value, na.rm = TRUE) / sum(area, na.rm = TRUE)
  )

X_region <- with(perturbations_sif, sparseMatrix(
  i = as.integer(year_month),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(year_month), nlevels(basis_vector))
))

# NOTE: ignore the inventory component for now
# - lots of zeros in daily files, so monthly product values are small and skewed toward zero
# - need decide how we produce the SIF prior... maybe reuse the daily solar noon inventory and averagee to months?
# - this would help remove all the zeros over ocean cells

# fn <- ncdf4::nc_open(args$inventory)
# on.exit(ncdf4::nc_close(fn))
# v <- function(...) ncdf4::ncvar_get(fn, ...)

# prior_sif <- expand.grid(
#   longitude = as.vector(v('lon')),
#   latitude = as.vector(v('lat')),
#   time = ncvar_get_time(fn, 'time'),
#   stringsAsFactors = FALSE
# ) %>%
#   mutate(
#     value = as.vector(v('sif')),
#     time = as.Date(time),
#     year_month = factor(time)
#   ) %>%
#   # inner_join(
#   #   perturbations_base %>%
#   #     distinct(longitude, latitude, time, area),
#   #   by = c('longitude', 'latitude', 'time')
#   # ) %>%
#   group_by(year_month) %>%
#   summarise(
#     # value = sum(area * value, na.rm = TRUE) / sum(area, na.rm = TRUE),
#     value = mean(value, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   mutate(output = 'Bottom-up') %>%
#   arrange(year_month)

posterior_sif <- tibble(
  output = 'Posterior',
  time = as.Date(levels(perturbations_sif$year_month)),
  # value_prior = value,
  value = as.vector(
    X_region[, as.integer(samples$alpha_df$basis_vector)]
    %*% samples$alpha_df$value
  ),
  value_samples = as.matrix(
    X_region[, as.integer(samples$alpha_df$basis_vector)]
    %*% samples$alpha_df$value_samples
  ),
  value_q025 = matrixStats::rowQuantiles(value_samples, probs = 0.025),
  value_q975 = matrixStats::rowQuantiles(value_samples, probs = 0.975)
) %>%
  select(-value_samples)
# select(-c(value_prior, value_samples))

# sif <- bind_rows(prior_sif, posterior_sif) %>%
#   mutate(time = as.Date(year_month))

output <- ggplot(posterior_sif, aes(time)) +
  geom_line(
    mapping = aes(
      y = value,
      colour = output,
      linetype = output
    ),
    linewidth = 0.4
  ) +
  geom_ribbon(
    mapping = aes(
      ymin = value_q025,
      ymax = value_q975,
      fill = output
    ),
    alpha = 0.3
  ) +
  scale_colour_manual(values = c('#ff4444')) +
  scale_fill_manual(values = c('#ff4444')) +
  scale_linetype_manual(values = c('41', 'solid')) +
  scale_x_date(date_breaks = '6 months', date_labels = '%Y-%m') +
  labs(
    title = 'Monthly global mean SIF',
    subtitle = 'Adjustment component only (i.e., this is the posterior minus the SIF prior mean)',
    x = 'Time', 
    y = expression('SIF [' * W * m^{-2} * µm^{-1} * sr^{-1} *']'),
    colour = NULL, 
    fill = NULL, 
    linetype = NULL) +
  guides(fill = 'none') +
  theme(
    plot.margin = margin(t = 1, r = 1, b = 0, l = 1, unit = 'mm'),
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggsave_base(
  '6_results_sif/figures/sif-posterior-global-monthly-free-resp-LNLGISSIF.pdf',
  output,
  width = 17,
  height = 11
)
