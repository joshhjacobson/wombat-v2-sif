library(dplyr)
library(ggplot2)

observations <- fst::read_fst('4_inversion/intermediates/osse-observations-zero.fst')

observations %>% glimpse

samp_size = 10000

ggplot(
  observations %>%
  sample_n(samp_size),
  aes(x = time, y = error, color = component_name)
) +
  geom_point(shape = 1, alpha = 0.2)

ggplot(
  observations %>%
  sample_n(samp_size),
  aes(x = time, y = epsilon, color = component_name)
) +
  geom_point(shape = 1, alpha = 0.2)

ggplot(
  observations %>%
  sample_n(samp_size),
  aes(x = time, y = value, color = component_name)
) +
  geom_point(shape = 1, alpha = 0.2) +
  facet_wrap(~ component_name, scales = 'free_y', ncol = 1)

ggplot(
  observations %>%
  sample_n(samp_size),
  aes(x = epsilon, fill = component_name)
) +
  geom_histogram() +
  facet_wrap(~ component_name, scales = 'free')

ggplot(
  observations %>%
  sample_n(samp_size),
  aes(x = value, fill = component_name)
) +
  geom_histogram() +
  facet_wrap(~ component_name, scales = 'free')


observations %>% filter(component_name == 'SIF') %>% nrow


ggplot(
  observations %>%
  filter(component_name == 'SIF') %>%
  sample_n(samp_size),
  aes(x = longitude, y = latitude)
) +
  geom_point(shape = 1, alpha = 0.2)
