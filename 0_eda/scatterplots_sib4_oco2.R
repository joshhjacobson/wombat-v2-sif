library(dplyr)
library(ggplot2)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)

# Read in SiB4 and OCO-2 data
oco2_sif <- fst::read.fst("data/eda/oco2/oco2_sif_2014-2020_daily_1deg.fst") %>% 
    as_tibble() %>% 
    select(date, longitude, latitude, value_daily) %>%
    rename(lon = longitude, lat = latitude, oco_sif = value_daily) %>% 
    mutate(month = clock::get_month(date)) %>%
    group_by(lon, lat, month) %>%
    nest() %>%
    ungroup()

nested_summaries <- readr::read_rds("data/eda/sib4/summaries_grid_month.rds")


# Join SiB4 and OCO-2 data
df <- nested_summaries %>%
    select(lon, lat, month, data) %>%
    inner_join(oco2_sif, by = c("lon", "lat", "month")) %>% 
    rename(data_sib4 = data.x, data_oco2 = data.y)
df

# Extract summaries and combine into a single tibble
df_intercept <- nested_summaries %>%
    unnest(cols = c(tidy_out)) %>%
    filter(term == "(Intercept)") %>%
    select(c(lon, lat, month, estimate))

df_slope <- nested_summaries %>%
    unnest(cols = c(tidy_out)) %>%
    filter(term == "assim") %>%
    select(c(lon, lat, month, estimate))

df_sigma <- nested_summaries %>%
    unnest(cols = c(glance_out)) %>%
    select(c(lon, lat, month, sigma))

df_rsquared <- nested_summaries %>%
    unnest(cols = c(glance_out)) %>%
    select(c(lon, lat, month, adj.r.squared))

df_fit_metrics <- list(df_intercept, df_slope, df_sigma, df_rsquared) %>%
    purrr::reduce(inner_join, by = c("lon", "lat", "month")) %>%
    rename(intercept = estimate.x, slope = estimate.y, std_err = sigma)

