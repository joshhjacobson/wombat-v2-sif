library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--perturbations-augmented')
parser$add_argument('--alpha-v2')
parser$add_argument('--alpha-positive')
parser$add_argument('--alpha-negative')
parser$add_argument('--output')
args <- parser$parse_args()

perturbations_augmented <- fst::read_fst(args$perturbations_augmented)
alpha_v2 <- fst::read_fst(args$alpha_v2)
alpha_positive <- fst::read_fst(args$alpha_positive)
alpha_negative <- fst::read_fst(args$alpha_negative)

perturbations_augmented <- perturbations_augmented %>%
  mutate(
    inventory_time = interaction(
      inventory,
      time,
      drop = TRUE
    )
  )

perturbations <- perturbations_augmented %>%
  group_by(inventory_time, basis_vector) %>%
  summarise(
    value = KG_M2_S_TO_PGC_MONTH * sum(area * value),
    .groups = 'drop'
  ) %>%
  left_join(
    perturbations_augmented %>%
      distinct(inventory_time, inventory, time),
    by = 'inventory_time'
  )

X_global <- with(perturbations, sparseMatrix(
  i = as.integer(inventory_time),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(inventory_time), nlevels(basis_vector))
))

bottom_up <- perturbations %>%
  group_by(inventory_time, inventory, time) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  select(-inventory_time) %>%
  mutate(output = 'Bottom-up')

true_emissions_alpha_v2 <- bottom_up %>%
  mutate(
    output = 'v2.0 post. mean',
    value = value + as.vector(
      X_global[, as.integer(alpha_v2$basis_vector)]
      %*% alpha_v2$value
    )
  )

true_emissions_alpha_positive <- bottom_up %>%
  mutate(
    output = 'v2.0 adj. pos.',
    value = value + as.vector(
      X_global[, as.integer(alpha_positive$basis_vector)]
      %*% alpha_positive$value
    )
  )

true_emissions_alpha_negative <- bottom_up %>%
  mutate(
    output = 'v2.0 adj. neg.',
    value = value + as.vector(
      X_global[, as.integer(alpha_negative$basis_vector)]
      %*% alpha_negative$value
    )
  )

emissions <- bind_rows(
  bottom_up,
  true_emissions_alpha_v2,
  true_emissions_alpha_positive,
  true_emissions_alpha_negative
) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
        filter(inventory %in% c('bio_assim', 'bio_resp_tot')) %>%
        group_by(output, time) %>%
        summarise(
          value = sum(value),
          .groups = 'drop'
        ) %>%
        mutate(inventory = 'nee'),
      x %>%
        group_by(output, time) %>%
        summarise(
          value = sum(value),
          .groups = 'drop'
        ) %>%
        mutate(inventory = 'total')
    )
  } %>%
  mutate(
    inventory = factor(c(
      'bio_assim' = 'GPP',
      'bio_resp_tot' = 'Respiration',
      'nee' = 'NEE',
      'ocean' = 'Ocean',
      'total' = 'Total'
    )[inventory], levels = c(
      'GPP',
      'Respiration',
      'NEE',
      'Ocean',
      'Total'
    )),
    output = factor(
      output,
      levels = c('Bottom-up', 'v2.0 post. mean', 'v2.0 adj. pos.', 'v2.0 adj. neg.')
    )
  )

colour_key <- c(
  'Bottom-up' = 'grey50',
  'v2.0 post. mean' = '#5954d6',
  'v2.0 adj. pos.' = '#00c6f8',
  'v2.0 adj. neg.' = '#ff9287'
)
linetype_key <- c(
  'Bottom-up' = '11',
  'v2.0 post. mean' = 'solid',
  'v2.0 adj. pos.' = '41',
  'v2.0 adj. neg.' = '41'
)

output <- ggplot(
  emissions %>% filter(inventory != 'Total'),
  aes(time)
) +
  geom_line(
    mapping = aes(
      y = value,
      colour = output,
      linetype = output
    ),
    linewidth = 0.6,
    alpha = 0.8
  ) +
  facet_wrap(vars(inventory), scales = 'free_y', nrow = 2) +
  scale_x_date(date_labels = '%Y-%m') +
  scale_colour_manual(values = colour_key) +
  scale_linetype_manual(values = linetype_key) +
  labs(x = 'Time', y = 'Flux [PgC/month]', colour = NULL, linetype = NULL) +
  theme(
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = 'mm'),
    plot.title = element_blank(),
    legend.position = 'right',
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm'),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(size = 9)
  )

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$supplement_full_width,
  height = 10.2
)
