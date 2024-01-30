library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(furrr)
library(lubridate, warn.conflicts = FALSE)
library(parallel)
library(tidyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--input-list')
parser$add_argument('--parallel-strategy')
parser$add_argument('--output')
args <- parser$parse_args()


plan(args$parallel_strategy, workers = get_cores())
options <- furrr_options(packages = c('dplyr', 'broom'))

input_paths <- strsplit(args$input_list, " ")[[1]]

inventory <- bind_rows(mclapply(input_paths, function(path) {
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

log_info('Fitting SIF-GPP linear models and computing SIF outlier statistics')
models <- inventory %>% 
  mutate(month = month(time)) %>%
  nest(.by = c(longitude, latitude, month)) %>%
  mutate(count = future_map_int(data, function(data) {
    sum(data$sif > 0.1)
  })) %>% 
  filter(count >= 30) %>%
  mutate(model = future_map(data, function(data) {
    fit <- broom::tidy(lm(sif ~ assim, data = data), .options = options)
    tibble(
      intercept = fit$estimate[1],
      slope = fit$estimate[2],
      pvalue_slope = fit$p.value[2]
    )
  })) %>% 
  mutate(fence = future_map(data, function(data) {
    quantiles <- quantile(data$sif, c(0.25, 0.75))
    iqr <- quantiles[2] - quantiles[1]
    tibble(
      lower_fence = quantiles[1] - 1.5 * iqr,
      upper_fence = quantiles[2] + 1.5 * iqr
    )
  })) %>% 
  select(-data) %>%
  unnest(cols = c(model, fence))

log_info('Saving')
fst::write_fst(models, args$output)

log_info('Done')