library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(furrr)
library(lubridate, warn.conflicts = FALSE)
library(parallel)
library(tidyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--input', nargs = '+')
parser$add_argument('--parallel-strategy')
parser$add_argument('--output')
args <- parser$parse_args()

plan(args$parallel_strategy, workers = get_cores())
shared_packages <- furrr_options(packages = c('dplyr', 'broom'))

SIGNIFICANCE_LEVEL <- 0.001

inventory <- bind_rows(mclapply(args$input, function(path) {
  log_trace('Opening {path}')
  fn <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(fn))
  v <- function(...) ncdf4::ncvar_get(fn, ...)
  expand.grid(
    longitude = as.vector(v('lon')),
    latitude = as.vector(v('lat')),
    time = ncvar_get_time(fn, 'time'),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      sif = as.vector(v('sif')),
      assim = as.vector(v('assim')),
      # The below equation for local time assumes time is in UTC
      local_hour = hour(time + hours(round(longitude / 15)))
    ) %>% 
    filter(between(local_hour, 12, 15))
}, mc.cores = get_cores())) %>% as_tibble()

log_info('Fitting SIF-ASSIM linear models and computing SIF outlier statistics')
models <- inventory %>% 
  mutate(month = month(time)) %>%
  nest(.by = c(longitude, latitude, month)) %>%
  mutate(count = future_map_int(data, function(data) {
    sum(data$sif > 0.1)
  }, .options = shared_packages)) %>% 
  filter(count >= 30) %>%
  mutate(model = future_map(data, function(data) {
    fit_linear <- lm(sif ~ assim, data = data)
    fit_poly <- lm(sif ~ poly(assim, degree = 3, raw = TRUE), data = data)
    metrics <- broom::tidy(fit_linear)
    tibble(
      intercept = metrics$estimate[1],
      slope = metrics$estimate[2],
      pvalue_slope = metrics$p.value[2],
      pvalue_poly = anova(fit_linear, fit_poly)$Pr[2],
      correlation = cor(data$assim, data$sif)
    )
  }, .options = shared_packages)) %>%
  unnest(cols = c(model)) %>%
  filter(
    correlation > 0.5,
    pvalue_poly > SIGNIFICANCE_LEVEL,
    intercept > -0.6
  ) %>%
  mutate(slope = if_else(pvalue_slope < SIGNIFICANCE_LEVEL, slope, 0)) %>%
  mutate(tukey_fence = future_map(data, function(data) {
    quantiles <- quantile(data$sif, c(0.25, 0.75))
    iqr <- quantiles[2] - quantiles[1]
    tibble(
      lower_fence = quantiles[1] - 1.5 * iqr,
      upper_fence = quantiles[2] + 1.5 * iqr
    )
  }, .options = shared_packages)) %>% 
  unnest(cols = c(tukey_fence))

log_info('Writing fitted models to {args$output}')
saveRDS(models, "3_sif/intermediates/models-data.rds")
models %>% select(-data) %>% fst::write_fst(args$output)

log_info('Done')