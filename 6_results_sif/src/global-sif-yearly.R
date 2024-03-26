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
args$samples <- '4_inversion/intermediates/samples-LNLGISSIF.rds'
args$region_sf <- '5_results/intermediates/region-sf.rds'


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
    # NOTE: sign of perturbations are switched to match SIF
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
  summarise(value = sum(area * value) / sum(area))

X_region <- with(perturbations_sif, sparseMatrix(
  i = as.integer(year_month),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(year_month), nlevels(basis_vector))
))

fn <- ncdf4::nc_open(args$inventory)
on.exit(ncdf4::nc_close(fn))
v <- function(...) ncdf4::ncvar_get(fn, ...)

prior_sif <- expand.grid(
  longitude = as.vector(v('lon')),
  latitude = as.vector(v('lat')),
  time = ncvar_get_time(fn, 'time'),
  stringsAsFactors = FALSE
) %>%
  mutate(
    value = as.vector(v('sif')),
    time = as.Date(time),
    year_month = factor(time)
  ) %>%
  inner_join(
    perturbations_base %>%
      distinct(longitude, latitude, time, area),
    by = c('longitude', 'latitude', 'time')
  ) %>%
  group_by(year_month) %>%
  summarise(value = sum(area * value) / sum(area), .groups = 'drop') %>%
  select(-year_month) %>%
  mutate(output = 'Bottom-up')

posterior_sif <- prior_sif %>%
  mutate(
    output = sprintf('Posterior'),
    value_prior = value,
    value = value_prior + as.vector(
      X_region[, as.integer(samples$alpha_df$basis_vector)]
      %*% samples$alpha_df$value
    ),
    value_samples = value_prior + as.matrix(
      X_region[, as.integer(samples$alpha_df$basis_vector)]
      %*% samples$alpha_df$value_samples
    ),
    value_q025 = matrixStats::rowQuantiles(value_samples, probs = 0.025),
    value_q975 = matrixStats::rowQuantiles(value_samples, probs = 0.975)
  ) %>%
  select(-value_prior)

sif <- bind_rows(prior_sif, posterior_sif)
