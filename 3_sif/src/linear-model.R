library(broom)
library(dplyr, warn.conflicts = FALSE)
library(furrr)
library(lubridate, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--input')
parser$add_argument('--region-mask')
parser$add_argument('--output')
args <- parser$parse_args()

# Identify land regions using region mask

# The region mask used will depend on the spatial resolution of the data; wait on this step.

# Suppose we have a tibble called input_land with columns (lon, lat, time, sif, assim) consisting of ONLY land locations (no missing values)

plan(multicore, workers = get_cores())
options <- furrr_options(packages = c("dplyr", "broom"))

# NOTE: Also collect details for robust outlier detection here

log_info('Fitting linear models')
models <- input_land %>%
  mutate(month = format(time, '%Y%m')) %>%
  select(-time) %>%
  group_by(lon, lat, month) %>%
  tidyr::nest() %>%
  ungroup() %>%
  mutate(model = future_map(data, function(x) {
    tidy(lm(sif ~ assim, data = x), .options = options)
  })) %>%
  tidyr::unnest(cols = model) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  rename(
    intercept = '(Intercept)',
    slope = 'assim'
  )

log_info('Saving')
fst::write_fst(models, args$output)

log_info('Done')
  