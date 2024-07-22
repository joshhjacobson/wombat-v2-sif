# source(Sys.getenv('UTILS_PARTIAL'))
# source(Sys.getenv('DISPLAY_PARTIAL'))

source('partials/utils.R')
source('partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(ggrepel)
library(lubridate, warn.conflicts = FALSE)
library(patchwork)

# parser <- ArgumentParser()
# parser$add_argument('--samples')
# parser$add_argument('--output-base')
# args <- parser$parse_args()

args <- list(
  samples = '4_inversion/intermediates/osse-samples-ALPHASMALL-FREERESP-FIXLW-WSIF.rds',
  output_base = '6_results_sif/figures/archive/osse/traceplots/traceplots-ALPHASMALL-FREERESP-FIXLW-WSIF'
)


samples <- readRDS(args$samples)

plot_traces <- function(x, names) {
  is_matrix <- is.matrix(x)
  if (!is_matrix) x <- t(t(x))
  iterations <- (samples$n_warm_up + 1) : samples$n_samples
  df <- data.frame(
    iteration = rep(iterations, ncol(x)),
    value = as.vector(x[iterations, ])
  )

  if (!missing(names)) {
    df$name <- rep(names, each = samples$n_samples - samples$n_warm_up)

    output <- ggplot(mapping = aes(iteration, value, colour = name))
  } else {
    output <- ggplot(mapping = aes(iteration, value))
  }

  output <- output +
    geom_line(data = df)

  if (!missing(names)) {
    label_iteration <- round((samples$n_warm_up + 1 + samples$n_samples) / 2)
    df <- data.frame(
      name = names,
      iteration = label_iteration
    )
    df$value <- if (dim(x)[2] > 1) colMeans(x[iterations, ]) else mean(x[iterations])
    output <- output + if (length(names) > 2) {
      geom_label_repel(
        data = df,
        size = 3,
        mapping = aes(label = name),
        direction = 'x',
        seed = 3
      )
    } else {
      geom_label(
        data = df,
        size = 3,
        mapping = aes(label = name)
      )
    }
  }

  output +
    guides(colour = 'none') +
    scale_y_continuous(expand = expansion(mult = 0.15)) +
    labs(x = 'Iteration', y = 'Value', colour = NULL) +
    theme(
      plot.margin = margin(t = 1, r = 2, b = 0, l = 0, unit = 'mm')
    )

}

to_bio_labels <- function(x) {
  c('bio_assim' = 'gpp', 'bio_resp_tot' = 'resp')[colnames(x)]
}

scale_assim_inventory <- scale_colour_manual(values = c('#018571'))
scale_bio_inventory <- scale_colour_manual(
  values = c('#018571', '#a6611a')
)
scale_natural_inventory <- scale_colour_manual(
  values = c(gpp = '#018571', resp = '#a6611a', ocean = 'blue')
)

# Hyperparameters
traces_hyperparameters <- wrap_plots(
  plot_traces(samples$w_bio_clim, to_bio_labels(samples$w_bio_clim)) +
    ggtitle(expression(tau[c]^beta)) +
    scale_bio_inventory,
  plot_traces(samples$rho_bio_clim) +
    ggtitle(expression(rho['gpp,resp']^beta)),
  plot_traces(samples$w_bio_resid, to_bio_labels(samples$w_bio_resid)) +
    ggtitle(expression(tau[c]^epsilon)) +
    scale_bio_inventory,
  plot_traces(samples$rho_bio_resid) +
    ggtitle(expression(rho['gpp,resp']^epsilon)),
  plot_traces(samples$kappa_bio_resid) +
    ggtitle(expression(kappa['bio']^epsilon)),
  plot_traces(
    samples$gamma,
    c(
      '1_LNLG' = 'OCO-2 land',
      'shipboard' = 'Shipboard',
      'tower' = 'Tower',
      'surface' = 'Surface',
      'aircraft' = 'Aircraft',
      '3_SIF' = 'OCO-2 SIF'
    )[colnames(samples$gamma)]
  ) +
    scale_colour_brewer(palette = 'Dark2') +
    ggtitle(expression(gamma[g]^Z)),
  ncol = 2
)
ggsave_base(
  paste0(args$output_base, '-hyperparameters.pdf'),
  traces_hyperparameters,
  width = DISPLAY_SETTINGS$supplement_full_width,
  height = 15
)

# Linear Components
components <- c('intercept', 'trend')
traces <- list()
if (
  samples$alpha_df %>%
    filter(inventory == 'bio_resp_tot', component %in% components) %>%
    nrow() > 0
) {
  inventories <- c('bio_assim', 'bio_resp_tot')
  for (r in seq(11)[-3]) {
    for (j in seq_along(components)) {
      r_j <- as.integer(r + (j - 1) * 11)
      region <- sprintf('Region%02d', r)
      traces[[r_j]] <- plot_traces(
        samples$alpha[, sprintf('%s.%s.%s', inventories, components[j], region)],
        c('gpp', 'resp')
      ) +
        ggtitle(bquote(alpha[.(sprintf('c,%01d,%01d', j - 1, r))])) +
        scale_bio_inventory
    }
  }
  for (j in seq_along(components)) {
    r_j <- as.integer(3 + (j - 1) * 11)
    traces[[r_j]] <- plot_traces(
      samples$alpha[, sprintf('bio_assim.%s.Region03', components[j])],
      'gpp'
    ) +
      ggtitle(bquote(alpha[.(sprintf('c,%01d,3', j - 1))])) +
      scale_bio_inventory
  }
} else {
  inventories <- c('bio_assim')
  for (r in 1:11) {
    for (j in seq_along(components)) {
      r_j <- as.integer(r + (j - 1) * 11)
      region <- sprintf('Region%02d', r)
      traces[[r_j]] <- plot_traces(
        samples$alpha[, sprintf('%s.%s.%s', inventories, components[j], region)],
        'gpp'
      ) +
        ggtitle(bquote(alpha[.(sprintf('c,%01d,%01d', j - 1, r))])) +
        scale_assim_inventory
    }
  }
}
traces_intercept_slope <- wrap_plots(traces, ncol = 2, byrow = FALSE)

ggsave_base(
  paste0(args$output_base, '-alpha01.pdf'),
  traces_intercept_slope,
  width = DISPLAY_SETTINGS$supplement_full_width,
  height = 60
)

# Seasonal Components (cosine)
inventories <- c('bio_assim', 'bio_resp_tot')
components <- c('intercept', 'trend')

for (k in 1:3) {
  traces <- list()
  for (r in 1:11) {
    for (j in seq_along(components)) {
      r_j <- as.integer(r + (j - 1) * 11)
      region <- sprintf('Region%02d', r)
      traces[[r_j]] <- plot_traces(
        samples$alpha[, sprintf('%s.cos12_%01d_%s.%s', inventories, k, components[j], region)],
        c('gpp', 'resp')
      ) +
        ggtitle(bquote(alpha[.(sprintf('c,%01d,%01d,%01d', j + 1, k, r))])) +
        scale_bio_inventory
    }
  }
  traces_cos <- wrap_plots(traces, ncol = 2, byrow = FALSE)

  ggsave_base(
    sprintf('%s-alpha-cos-%01d.pdf', args$output_base, k),
    traces_cos,
    width = DISPLAY_SETTINGS$supplement_full_width,
    height = 60
  )
}

# Seasonal Components (sine)
inventories <- c('bio_assim', 'bio_resp_tot')
components <- c('intercept', 'trend')

for (k in 1:3) {
  traces <- list()
  for (r in 1:11) {
    for (j in seq_along(components)) {
      r_j <- as.integer(r + (j - 1) * 11)
      region <- sprintf('Region%02d', r)
      traces[[r_j]] <- plot_traces(
        samples$alpha[, sprintf('%s.sin12_%01d_%s.%s', inventories, k, components[j], region)],
        c('gpp', 'resp')
      ) +
        ggtitle(bquote(alpha[.(sprintf('c,%01d,%01d,%01d', j + 3, k, r))])) +
        scale_bio_inventory
    }
  }
  traces_sin <- wrap_plots(traces, ncol = 2, byrow = FALSE)

  ggsave_base(
    sprintf('%s-alpha-sin-%01d.pdf', args$output_base, k),
    traces_sin,
    width = DISPLAY_SETTINGS$supplement_full_width,
    height = 60
  )
}


# Residual Components
inventories <- c('bio_assim', 'bio_resp_tot', 'ocean')
regions <- c(2, 3, 6, 7, 9, 11, 12, 18, 22)
months <- c(5, 20, 40, 60, 75)

traces <- list()
i <- 1
for (r in seq_along(regions)) {
  for (m in seq_along(months)) {
    region <- sprintf('Region%02d', regions[r])
    month <- format(as.Date('2014-08-01') + months(months[m]), '%Y-%m')
    traces[[i]] <- plot_traces(
      samples$alpha[, sprintf('%s.residual.%s.%s', inventories, region, month)],
      c('gpp', 'resp', 'ocean')
    ) +
      ggtitle(bquote(alpha[.(sprintf('c,6,%01d,%01d', regions[r], months[m]))])) +
      scale_natural_inventory

    i <- i + 1
  }
}
traces_resid <- wrap_plots(traces, ncol = length(months), byrow = TRUE)

ggsave_base(
  sprintf('%s-alpha-resid.pdf', args$output_base),
  traces_resid,
  width = 2 * DISPLAY_SETTINGS$supplement_full_width,
  height = 60
)
