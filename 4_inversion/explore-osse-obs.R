library(dplyr)
library(ggplot2)

observations <- fst::read_fst('4_inversion/intermediates/osse-observations-ALPHAV2.fst')
observations_alpha0 <- fst::read_fst('4_inversion/intermediates/osse-observations-ALPHA0.fst')

observations %>% glimpse
observations$hyperparameter_group %>% unique

samp_size = 10000

ggplot(
  observations %>%
  sample_n(samp_size),
  aes(x = time, y = error, color = hyperparameter_group)
) +
  geom_point(shape = 1, alpha = 0.2)

ggplot(
  observations %>%
  sample_n(samp_size),
  aes(x = time, y = epsilon, color = hyperparameter_group)
) +
  geom_point(shape = 1, alpha = 0.2)

ggplot(
  observations_alpha0 %>%
  sample_n(samp_size),
  aes(x = time, y = epsilon, color = hyperparameter_group)
) +
  geom_point(shape = 1, alpha = 0.2)

ggplot(
  observations %>%
  sample_n(samp_size),
  aes(x = time, y = value, color = hyperparameter_group)
) +
  geom_point(shape = 1, alpha = 0.2) +
  facet_wrap(~ hyperparameter_group, scales = 'free_y', ncol = 1)

ggplot(
  observations %>%
  sample_n(samp_size),
  aes(x = epsilon, fill = hyperparameter_group)
) +
  geom_histogram() +
  facet_wrap(~ hyperparameter_group, scales = 'free')

ggplot(
  observations_alpha0 %>%
  sample_n(samp_size),
  aes(x = epsilon, fill = hyperparameter_group)
) +
  geom_histogram() +
  facet_wrap(~ hyperparameter_group, scales = 'free')

ggplot(
  observations %>%
  sample_n(samp_size),
  aes(x = value, fill = hyperparameter_group)
) +
  geom_histogram() +
  facet_wrap(~ hyperparameter_group, scales = 'free')


observations %>% filter(hyperparameter_group == 'SIF') %>% nrow


ggplot(
  observations %>%
  filter(hyperparameter_group == 'SIF') %>%
  sample_n(samp_size),
  aes(x = longitude, y = latitude)
) +
  geom_point(shape = 1, alpha = 0.2)
