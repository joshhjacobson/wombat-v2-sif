library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(patchwork)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--flux-aggregators')
parser$add_argument('--alpha-v2')
parser$add_argument('--alpha-free')
parser$add_argument('--output')
args <- parser$parse_args()

flux_aggregators <- fst::read_fst(args$flux_aggregators)
alpha_v2 <- fst::read_fst(args$alpha_v2)
alpha_free <- fst::read_fst(args$alpha_free)

perturbations_base <- flux_aggregators %>%
  filter(
    time >= '2015-01-01',
    time < '2021-01-01'
  ) %>%
  mutate(
    inventory_time = interaction(
      inventory,
      time,
      drop = TRUE
    )
  )

perturbations <- perturbations_base %>%
  group_by(inventory_time, basis_vector) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  left_join(
    perturbations_base %>%
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
    output = 'WOMBAT v2',
    value = value + as.vector(
      X_global[, as.integer(alpha_v2$basis_vector)]
      %*% alpha_v2$value
    )
  )

true_emissions_alpha_free <- bottom_up %>%
  mutate(
    output = 'WOMBAT v2, adj.',
    value = value + as.vector(
      X_global[, as.integer(alpha_free$basis_vector)]
      %*% alpha_free$value
    )
  )

emissions <- bind_rows(
  bottom_up,
  true_emissions_alpha_v2,
  true_emissions_alpha_free
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
      levels = c('Bottom-up', 'WOMBAT v2', 'WOMBAT v2, adj.')
    )
  )

colour_key <- c('Bottom-up' = 'grey50', 'WOMBAT v2' = '#4053d3', 'WOMBAT v2, adj.' = '#ddb310')
linetype_key <- c('Bottom-up' = '11', 'WOMBAT v2' = '41', 'WOMBAT v2, adj.' = '1131')

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
    linewidth = 0.4
  ) +
  facet_wrap(~ inventory, scales = 'free_y', nrow = 2) +
  scale_colour_manual(values = colour_key) +
  scale_linetype_manual(values = linetype_key) +
  labs(x = 'Time', y = 'Flux [PgC per month]', colour = NULL, linetype = NULL) +
  ggtitle('OSSE true flux components') +
  theme(
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = 'mm'),
    plot.title = element_text(size = 11, hjust = 0.5),
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
