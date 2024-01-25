# NOTE: begining to adapt from photobiology package

sun_angles <- function(time = lubridate::now(tzone = "UTC"),
                       tz = lubridate::tz(time),
                       geocode = tibble::tibble(lon = 0,
                                                lat = 51.5,
                                                address = "Greenwich"),
                       use.refraction = FALSE)
{
  stopifnot(lubridate::is.POSIXct(time))
  validate_coords(lon, lat)

  z <- list(nrow(geocode))
  for (i in seq_len(nrow(geocode))) {
    temp <- sun_angles_fast(time = time,
                            tz = tz,
                            geocode = dplyr::slice(geocode, i),
                            use.refraction = use.refraction)
    z[[i]] <- temp # needed so that class attribute is retained
  }
  z <- suppressWarnings(dplyr::bind_rows(z))
  class(z[["solartime"]]) <- class(temp[["solartime"]])

  # assertion
  if (any(z[["elevation"]] < (-90)) || any(z[["elevation"]] > 90))
    warning("Returned 'elevation' value(s) off range")
  if (any(!is.na(z[["azimuth"]]) & (z[["azimuth"]] < -1e-10 |
         z[["azimuth"]] > (360 + 1e-10)))) {
      warning("Returned 'azimuth' values(s) off range")
  }
  z
}


sun_angles_fast <- function(time, lon, lat)
{
  # Input validation done in sun_angles() before calling this function.
  # stopifnot(!anyNA(time))
  # stopifnot(is.data.frame(geocode))
  # stopifnot(nrow(geocode == 1) && length(tz == 1))
  # We have a single geocode and all times are expressed in the same time zone!
  # If time is a vector we can vectorize the whole calculation, and do the
  # expensive calculations only once.

  cent <- julian_century(time)

  sun.lon.mean <- geom_mean_lon_sun(cent)
  sun.anom.mean <- geom_mean_anom_sun(cent)
  eccent.earth <- eccent_earth_orbit(cent)
  delta <- sun_eq_of_ctr(cent, sun.anom.mean)

  sun.ecliptic <- mean_obliq_eclip(cent)
  obliq.corr <- obliq_corr(cent, sun.ecliptic)
  var.y <- var_y(obliq.corr)
  eq.of.time <- eq_of_time(mean.lon = sun.lon.mean,
                           eccent.earth = eccent.earth,
                           anom.mean = sun.anom.mean,
                           var.y = var.y)

  solar.time <- solar_tod(time, lat, lon, eq.of.time)
  solar.time <- solar.time / 60 # hours
  solar.time <- solar.time  %% 24 # needed for DST
  class(solar.time) <- c("solar_time", class(solar.time))

}


validate_coords <- function(lon, lat) {
  if (any(na.omit(lon) > 180 | na.omit(lon) < -180)) {
    stop("Longitude is out of range.")
  }
  if (any(na.omit(lat) > 89.99 | na.omit(lat) < -89.99)) {
    stop("Latitude is out of range.")
  }
}