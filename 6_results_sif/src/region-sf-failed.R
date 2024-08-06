source(Sys.getenv('UTILS_PARTIAL'))

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(sf)

parser <- ArgumentParser()
parser$add_argument('--region-grid')
parser$add_argument('--output')
args <- parser$parse_args()

region_grid <- readRDS(args$region_grid)

grid_latitude <- attr(region_grid$Region01, 'latitude')
grid_longitude <- attr(region_grid$Region01, 'longitude')
dy <- diff(grid_latitude[2:3])
dx <- diff(grid_longitude[2:3])

subset_grid <- function(x) t(x[
  seq_along(grid_longitude),
  2 : (length(grid_latitude) - 1)
])

region_values <- subset_grid(region_grid$Region01)
for (i in 2 : length(region_grid)) {
  x <- subset_grid(region_grid[[i]])
  region_values[x == 1] <- i
}
# region_values[region_values == 0] <- NA

# First grid cell is centred on -180, which makes it hard to construct
# compliant polygons; avoid this by splitting horizontal cells in two
region_values_wide <- matrix(
  0,
  nrow = nrow(region_values),
  ncol = 2 * ncol(region_values)
)
for (i in seq_len(nrow(region_values))) {
  # First original cell sits on the boundary, so it's value goes to first and
  # last new cell
  region_values_wide[i, 1] <- region_values[i, 1]
  region_values_wide[i, ncol(region_values_wide)] <- region_values[i, 1]
  # Remaining cells simply repeat
  region_values_wide[
    i,
    2 : (ncol(region_values_wide) - 1)
  ] <- rep(region_values[i, 2 : ncol(region_values)], each = 2)
}
region_values_wide[region_values_wide == 0] <- NA

region_raster <- stars::st_as_stars(t(region_values_wide)) %>%
  stars::st_set_dimensions(
    which = c('X1', 'X2'),
    names = c('longitude', 'latitude')
  ) %>%
  stars::st_set_dimensions(
    which = 'longitude',
    point = FALSE,
    values = seq(
      grid_longitude[1] + 0.25 * dx,
      grid_longitude[length(grid_longitude)] + 0.75 * dx,
      by = 0.5 * dx
    )
  ) %>%
  stars::st_set_dimensions(
    which = 'latitude',
    point = FALSE,
    values = seq(
      grid_latitude[2],
      grid_latitude[length(grid_latitude) - 1],
      by = dy
    )
  )
names(region_raster) <- 'region'
plot(region_raster)

region_sf <- region_raster %>%
  st_as_sf(as_point = FALSE, merge = TRUE) %>%
  mutate(
    region_code = if_else(
      region != 23,
      sprintf('T%02d', region),
      'NZ'
    )
  )
st_crs(region_sf) <- 'WGS84'

# ocean_region_colours <- region_sf %>%
#   filter(region >= 12, region != 23) %>%
#   # spdep::poly2nb() %>%
#   sfdep::st_contiguity() %>%
#   igraph::graph_from_adj_list() %>%
#   igraph::greedy_vertex_coloring()

# land_region_nb <- region_sf %>%
#   filter(between(region, 1, 11) | (region == 23)) %>%
#   # spdep::poly2nb()
#   sfdep::st_contiguity()
# land_region_nb[[2]] <- integer(0)
# land_region_nb[[5]] <- integer(0)

# land_region_colours <- land_region_nb %>%
#   igraph::graph_from_adj_list() %>%
#   igraph::greedy_vertex_coloring()

land_palette <- c(
  '#bbffbb', '#bae4b3',
  '#77ff77', '#31a354',
  '#99ff99', '#74c476'
)
ocean_palette <- c(
  '#bbbbff', '#bdd7e7',
  '#9999ff', '#6baed6',
  '#7777ff', '#3182bd'
)
region_colours <- data.frame(
  region_code = sprintf('T%02d', 0:23),
  colour = c(
    'white',
    land_palette[3],
    land_palette[1],
    land_palette[3],
    land_palette[1],
    land_palette[2],
    land_palette[1],
    land_palette[2],
    land_palette[1],
    land_palette[3],
    land_palette[1],
    land_palette[3],
    ocean_palette[2],
    ocean_palette[1],
    ocean_palette[3],
    ocean_palette[2],
    ocean_palette[1],
    ocean_palette[2],
    ocean_palette[1],
    ocean_palette[2],
    ocean_palette[3],
    ocean_palette[2],
    ocean_palette[1],
    land_palette[2]
  )
) %>%
  mutate(
    region_code = if_else(
      region_code != 'T23',
      region_code,
      'NZ'
    )
  )

# region_colours <- rep('white', 23)
# region_colours[with(region_sf,
#   region >= 12 & region != 23
# )] <- c(
#   '#bbbbff',
#   '#9999ff',
#   '#7777ff'
# )[ocean_region_colours]
# region_colours[with(region_sf,
#   (region >= 1 & region <= 11) | (region == 23)
# )] <- c(
#   '#bbffbb',
#   '#77ff77',
#   '#99ff99'
# )[land_region_colours]
# names(region_colours) <- region_sf$region_code
## region_colours['T10'] <- '#bbffbb'

region_sf <- region_sf %>%
  left_join(region_colours, by = 'region_code')

region_sf$label_nudge_x <- 0
region_sf$label_nudge_y <- 0

region_sf$label_nudge_y[region_sf$region_code == 'T08'] <- 5
region_sf$label_nudge_x[region_sf$region_code == 'T09'] <- 15
region_sf$label_nudge_y[region_sf$region_code == 'T09'] <- -10

saveRDS(region_sf, args$output)
