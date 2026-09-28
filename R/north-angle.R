north_transform <- function(points, crs) {
  transformed <- tryCatch(
    sf::st_transform(points, crs, partial = FALSE),
    error = function(error) {
      l_abort(
        "Cannot transform the north reference into the requested CRS.",
        "north_projection"
      )
    }
  )
  coordinates <- sf::st_coordinates(transformed)
  if (any(sf::st_is_empty(transformed)) || any(!is.finite(coordinates))) {
    l_abort(
      "The north reference is outside the projection domain.",
      "north_projection"
    )
  }
  coordinates[, c("X", "Y"), drop = FALSE]
}

#' Calculate the local true-north rotation for cartographic symbols
#'
#' Compute an angle reusable by [l_north_arrow()] and [l_north_rose()] from
#' the local direction of a geographic meridian in the displayed map CRS.
#'
#' @param map A single-panel coord_sf ggplot, an [l_frame()] (including an
#'   [l_inset()]), an sf bounding box with CRS, or a CRS specification accepted
#'   by [sf::st_crs()], such as an EPSG number, WKT string or crs object.
#' @param at Reference location. A numeric vector `c(longitude, latitude)` is
#'   always in WGS84 degrees, regardless of the map CRS. Alternatively supply
#'   one sf/sfc POINT with a known CRS. With `NULL`, use the center of the
#'   displayed extent for a plot, frame or bounding box. A CRS alone requires
#'   an explicit location. This is a position, not a layout anchor.
#' @param step Positive latitude offset in degrees, at most one; default
#'   `0.0001`. A symmetric difference at latitude plus/minus this offset
#'   approximates the local meridian tangent. Both samples must avoid the poles.
#'
#' @details
#' Requires optional sf, already used by the other cartographic constructors.
#' The result describes true north along the WGS84 meridian at the reference
#' location, not magnetic north or a magnetic declination. No device is opened,
#' no plot is drawn, and inputs and sf global settings are unchanged.
#'
#' The displayed CRS and aspect come from the built coord_sf panel or the
#' frame's saved map context. For a bbox or CRS alone, equal physical scaling
#' of the two coordinate axes is assumed. Rotation or nonuniform stretching
#' applied outside that map context is not included. A scalar angle is a local
#' approximation: north may point differently elsewhere in a large map.
#'
#' Invalid coordinates, unknown CRSs, poles, nonfinite transformations and
#' locally discontinuous or degenerate directions raise an lplot error instead
#' of returning an arbitrary angle. Geocentric CRSs are not supported. Locations
#' outside a frame's displayed extent are allowed if the projection is valid.
#'
#' @returns One finite numeric angle in degrees, in `[-180, 180)`. Zero points
#'   up; positive values rotate counterclockwise, exactly as required by the
#'   `angle` argument of [l_north_arrow()] and [l_north_rose()]. The value is
#'   computed now, not automatically updated when a source map changes.
#' @seealso [l_frame()], [l_inset()], [l_north_arrow()], [l_north_rose()]
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   angle <- l_north_angle(3413, at = c(0, 75))
#'   angle
#'   north <- l_north_arrow("minimal", angle = angle)
#'   rose <- l_north_rose("eight_point", angle = angle)
#'   l_render(l_viewport(list(
#'     l_place(north, left = 20, top = 20, width = 40, height = 68),
#'     l_place(rose, left = 100, top = 20, width = 100, height = 100)
#'   )))
#' }
#' @export
l_north_angle <- function(map, at = NULL, step = 0.0001) {
  require_cartography()
  scalar_number(step, "step", positive = TRUE)
  if (step > 1) {
    l_abort("step must be at most one degree.", property = "step")
  }
  extent <- NULL
  horizontal_scale <- 1
  if (inherits(map, "l_frame") || inherits(map, "ggplot")) {
    context <- if (inherits(map, "l_frame")) {
      map$map_context
    } else {
      map_panel_context(map)
    }
    extent <- context$extent
    crs <- context$crs
    horizontal_scale <- context$aspect_ratio *
      as.numeric(extent[["ymax"]] - extent[["ymin"]]) /
      as.numeric(extent[["xmax"]] - extent[["xmin"]])
  } else {
    crs <- tryCatch(sf::st_crs(map), error = function(error) {
      l_abort("map must provide a valid CRS.", property = "map")
    })
    if (inherits(map, "bbox")) extent <- map
  }
  if (
    is.na(crs) ||
      grepl("CS[Cartesian,3]", gsub("[[:space:]]", "", crs$wkt), fixed = TRUE)
  ) {
    l_abort(
      "A known geographic or projected CRS is required.",
      property = "map"
    )
  }
  if (
    !is.null(extent) &&
      (any(!is.finite(extent)) ||
        extent[["xmin"]] >= extent[["xmax"]] ||
        extent[["ymin"]] >= extent[["ymax"]])
  ) {
    l_abort(
      "The reference extent must have finite increasing bounds.",
      property = "map"
    )
  }
  if (is.null(at)) {
    if (is.null(extent)) {
      l_abort("A CRS alone requires an explicit at location.", property = "at")
    }
    at <- sf::st_sfc(
      sf::st_point(c(
        mean(as.numeric(extent[c("xmin", "xmax")])),
        mean(as.numeric(extent[c("ymin", "ymax")]))
      )),
      crs = crs
    )
  }
  if (inherits(at, c("sf", "sfc"))) {
    geometry <- sf::st_geometry(at)
    if (
      length(geometry) != 1L ||
        is.na(sf::st_crs(geometry)) ||
        !inherits(geometry[[1L]], "POINT") ||
        sf::st_is_empty(geometry)
    ) {
      l_abort(
        "at must contain one nonempty POINT with a known CRS.",
        property = "at"
      )
    }
    location <- as.numeric(north_transform(geometry, "OGC:CRS84")[1L, ])
  } else {
    if (
      !is.numeric(at) ||
        !is.null(dim(at)) ||
        length(at) != 2L ||
        any(!is.finite(at))
    ) {
      l_abort(
        "at must be c(longitude, latitude) or one sf POINT.",
        property = "at"
      )
    }
    location <- unname(at)
  }
  if (abs(location[[1L]]) > 180 || abs(location[[2L]]) + step >= 90) {
    l_abort(
      "Longitude must be within [-180, 180] and latitude samples must avoid the poles.",
      property = "at"
    )
  }
  samples <- sf::st_sfc(
    lapply(c(-step, 0, step), function(offset) {
      sf::st_point(location + c(0, offset))
    }),
    crs = "OGC:CRS84"
  )
  projected <- north_transform(samples, crs)
  south <- projected[2L, ] - projected[1L, ]
  north <- projected[3L, ] - projected[2L, ]
  if (sum(south * north) <= 0 || any(!is.finite(c(south, north)))) {
    l_abort(
      "True north is undefined or discontinuous at this reference.",
      "north_projection"
    )
  }
  direction <- projected[3L, ] - projected[1L, ]
  direction[[1L]] <- direction[[1L]] * horizontal_scale
  angle <- atan2(direction[[2L]], direction[[1L]]) * 180 / pi - 90
  as.numeric((angle + 180) %% 360 - 180)
}
