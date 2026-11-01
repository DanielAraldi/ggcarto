wgs84_semi_major <- 6378137
wgs84_flattening <- 1 / 298.257223563

# Ellipsoidal distance in metres between pairs of WGS 84 longitude/latitude
# coordinates (degrees), using the inverse formula of Vincenty (1975).
# Inputs are recycled vectors. Nonfinite inputs and pairs for which the
# iteration does not converge (nearly antipodal points) give NA.
geodesic_distance <- function(lon1, lat1, lon2, lat2) {
  a <- wgs84_semi_major
  f <- wgs84_flattening
  b <- a * (1 - f)
  radians <- pi / 180
  difference <- ((lon2 - lon1) * radians + pi) %% (2 * pi) - pi
  reduced1 <- atan((1 - f) * tan(lat1 * radians))
  reduced2 <- atan((1 - f) * tan(lat2 * radians))
  sin1 <- sin(reduced1)
  cos1 <- cos(reduced1)
  sin2 <- sin(reduced2)
  cos2 <- cos(reduced2)
  lambda <- difference
  converged <- rep(FALSE, length(lambda))
  for (iteration in seq_len(200L)) {
    sin_lambda <- sin(lambda)
    cos_lambda <- cos(lambda)
    sin_sigma <- sqrt(
      (cos2 * sin_lambda)^2 + (cos1 * sin2 - sin1 * cos2 * cos_lambda)^2
    )
    cos_sigma <- sin1 * sin2 + cos1 * cos2 * cos_lambda
    sigma <- atan2(sin_sigma, cos_sigma)
    sin_alpha <- ifelse(sin_sigma == 0, 0, cos1 * cos2 * sin_lambda / sin_sigma)
    cos2_alpha <- 1 - sin_alpha^2
    cos_2sigma_m <- ifelse(
      cos2_alpha == 0,
      0,
      cos_sigma - 2 * sin1 * sin2 / cos2_alpha
    )
    c_term <- f / 16 * cos2_alpha * (4 + f * (4 - 3 * cos2_alpha))
    previous <- lambda
    correction <- cos_2sigma_m + c_term * cos_sigma * (2 * cos_2sigma_m^2 - 1)
    correction <- sigma + c_term * sin_sigma * correction
    lambda <- difference + (1 - c_term) * f * sin_alpha * correction
    converged <- !is.na(lambda) & abs(lambda - previous) <= 1e-12
    if (all(converged | is.na(lambda))) {
      break
    }
  }
  distance <- b *
    vincenty_arc(sigma, sin_sigma, cos_sigma, cos_2sigma_m, cos2_alpha)
  distance[!converged] <- NA_real_
  distance
}

# Ellipsoidal correction of the auxiliary-sphere arc (Vincenty, 1975,
# equations 3, 4, 6 and 19), returned in units of the semi-minor axis.
vincenty_arc <- function(
  sigma,
  sin_sigma,
  cos_sigma,
  cos_2sigma_m,
  cos2_alpha
) {
  a <- wgs84_semi_major
  b <- a * (1 - wgs84_flattening)
  u2 <- cos2_alpha * (a^2 - b^2) / b^2
  big_a <- 1 + u2 / 16384 * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)))
  big_b <- u2 / 1024 * (256 + u2 * (-128 + u2 * (74 - 47 * u2)))
  inner <- big_b /
    6 *
    cos_2sigma_m *
    (4 * sin_sigma^2 - 3) *
    (4 * cos_2sigma_m^2 - 3)
  inner <- cos_sigma * (2 * cos_2sigma_m^2 - 1) - inner
  delta_sigma <- big_b * sin_sigma * (cos_2sigma_m + big_b / 4 * inner)
  big_a * (sigma - delta_sigma)
}

# Number of pieces into which each half of the reference line is divided.
scale_profile_pieces <- 256L

scale_geographic <- function(points, crs) {
  coordinates <- tryCatch(
    sf::sf_project(crs, "OGC:CRS84", points, keep = TRUE, warn = FALSE),
    error = function(error) {
      gc_abort(
        "The map CRS cannot be transformed to geographic coordinates.",
        "scale_projection"
      )
    }
  )
  invalid <- !is.finite(coordinates[, 1L]) | !is.finite(coordinates[, 2L])
  coordinates[invalid, ] <- NA_real_
  coordinates
}

# Cumulative ground length from the first point along a polyline given as a
# longitude/latitude matrix. The profile stops at the first point that cannot
# be projected or measured.
scale_cumulative_length <- function(coordinates) {
  count <- nrow(coordinates)
  pieces <- geodesic_distance(
    coordinates[-count, 1L],
    coordinates[-count, 2L],
    coordinates[-1L, 1L],
    coordinates[-1L, 2L]
  )
  invalid <- which(is.na(pieces))
  valid <- if (length(invalid)) invalid[[1L]] - 1L else length(pieces)
  c(0, cumsum(pieces[seq_len(valid)]))
}

# Ground length of horizontal map segments centred on the displayed extent.
# The reference line passes through the centre of the extent, parallel to the
# horizontal axis of the panel, which is where the bar's length is drawn.
# `half_widths` are offsets in map units from the centre; `lengths` are the
# geodesic lengths in metres of the segments spanning twice those offsets.
scale_ground_profile <- function(map) {
  extent <- as.numeric(map$extent[c("xmin", "ymin", "xmax", "ymax")])
  centre <- c(mean(extent[c(1L, 3L)]), mean(extent[c(2L, 4L)]))
  half <- (extent[[3L]] - extent[[1L]]) / 2
  pieces <- scale_profile_pieces
  offsets <- half * seq(0, 1, length.out = pieces + 1L)
  x <- c(centre[[1L]] - rev(offsets[-1L]), centre[[1L]] + offsets)
  coordinates <- scale_geographic(cbind(x, centre[[2L]]), map$crs)
  middle <- pieces + 1L
  if (anyNA(coordinates[middle, ])) {
    gc_abort(
      "The centre of the map is outside the projection domain.",
      "scale_projection"
    )
  }
  right <- scale_cumulative_length(coordinates[middle:nrow(coordinates), ])
  left <- scale_cumulative_length(coordinates[middle:1L, ])
  usable <- min(length(right), length(left))
  if (usable < 2L) {
    gc_abort(
      "The ground distance cannot be measured at the centre of the map.",
      "scale_projection"
    )
  }
  lengths <- right[seq_len(usable)] + left[seq_len(usable)]
  list(
    half_widths = offsets[seq_len(usable)],
    lengths = lengths,
    extent_half_width = half,
    extent_length = if (usable == pieces + 1L) lengths[[usable]] else NA_real_,
    reference = as.numeric(coordinates[middle, ])
  )
}

# Piecewise linear interpolation over a nondecreasing profile.
scale_interpolate <- function(from, to, value) {
  if (value <= from[[1L]]) {
    return(to[[1L]])
  }
  index <- findInterval(value, from, left.open = TRUE)
  if (index >= length(from)) {
    return(to[[length(to)]])
  }
  span <- from[[index + 1L]] - from[[index]]
  weight <- if (span > 0) (value - from[[index]]) / span else 0
  to[[index]] + weight * (to[[index + 1L]] - to[[index]])
}
