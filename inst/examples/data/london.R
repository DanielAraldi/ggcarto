london_crs <- 27700

london_require <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "This example needs sf. Run install.packages('sf') first.",
      call. = FALSE
    )
  }
  if (!requireNamespace("KernSmooth", quietly = TRUE)) {
    stop(
      "This example needs KernSmooth. ",
      "Run install.packages('KernSmooth') first.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

london_cache <- function(path = file.path(tempdir(), "ggcarto-london")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

london_download <- function(url, file, path) {
  destination <- file.path(path, file)
  if (!file.exists(destination)) {
    utils::download.file(url, destination, mode = "wb", quiet = TRUE)
  }
  destination
}

london_natural_earth <- function(category, layer, path) {
  archive <- london_download(
    paste0(
      "https://naciscdn.org/naturalearth/10m/",
      category,
      "/",
      layer,
      ".zip"
    ),
    paste0(layer, ".zip"),
    path
  )
  sf::st_read(paste0("/vsizip/", archive, "/", layer, ".shp"), quiet = TRUE)
}

london_crimes <- function(path) {
  archive <- london_download(
    "https://www.jstatsoft.org/index.php/jss/article/download/v084i06/3637",
    "crimes_in_Greater_London_2015-10.zip",
    path
  )
  files <- c(
    "2015-10-city-of-london-street.csv",
    "2015-10-metropolitan-street.csv"
  )
  crimes <- do.call(
    rbind,
    lapply(files, function(file) {
      utils::read.csv(unz(archive, file))[, c("Longitude", "Latitude")]
    })
  )
  crimes <- crimes[!is.na(crimes$Longitude) & !is.na(crimes$Latitude), ]
  sf::st_transform(
    sf::st_as_sf(crimes, coords = c("Longitude", "Latitude"), crs = 4326),
    london_crs
  )
}

london_boroughs <- function(path) {
  regions <- london_natural_earth(
    "cultural",
    "ne_10m_admin_1_states_provinces",
    path
  )
  boroughs <- regions[which(regions$region == "Greater London"), "name"]
  sf::st_make_valid(sf::st_transform(boroughs, london_crs))
}

london_thames <- function(extent, path) {
  rivers <- london_natural_earth(
    "physical",
    "ne_10m_rivers_lake_centerlines",
    path
  )
  rivers <- sf::st_transform(rivers[rivers$name == "Thames", ], london_crs)
  sf::st_intersection(
    sf::st_geometry(rivers),
    sf::st_as_sfc(extent)
  )
}

london_extent <- function(boroughs, margin = 0.018) {
  bounds <- sf::st_bbox(boroughs)
  padding <- c(
    diff(bounds[c("xmin", "xmax")]),
    diff(bounds[c("ymin", "ymax")])
  ) *
    margin
  bounds[c("xmin", "ymin")] <- bounds[c("xmin", "ymin")] - padding
  bounds[c("xmax", "ymax")] <- bounds[c("xmax", "ymax")] + padding
  bounds
}

london_density <- function(crimes, extent, bandwidth = 500, cell = 100) {
  coordinates <- sf::st_coordinates(crimes)
  range <- list(
    as.numeric(extent[c("xmin", "xmax")]),
    as.numeric(extent[c("ymin", "ymax")])
  )
  size <- vapply(range, function(axis) {
    as.integer(ceiling(diff(axis) / cell))
  }, integer(1))
  estimate <- KernSmooth::bkde2D(
    coordinates,
    bandwidth = c(bandwidth, bandwidth),
    gridsize = size,
    range.x = range
  )
  grid <- expand.grid(x = estimate$x1, y = estimate$x2)
  grid$density <- as.vector(estimate$fhat) * nrow(coordinates) * 1e6
  grid
}

london_mask <- function(boroughs, extent) {
  outside <- sf::st_as_sfc(extent)
  sf::st_difference(outside, sf::st_union(boroughs))
}

london_data <- function(path = london_cache()) {
  london_require()
  boroughs <- london_boroughs(path)
  extent <- london_extent(boroughs)
  crimes <- london_crimes(path)
  inside <- lengths(sf::st_intersects(crimes, sf::st_union(boroughs))) > 0
  crimes <- crimes[inside, ]
  list(
    extent = extent,
    boroughs = boroughs,
    thames = london_thames(extent, path),
    mask = london_mask(boroughs, extent),
    density = london_density(crimes, extent),
    crimes = nrow(crimes)
  )
}
