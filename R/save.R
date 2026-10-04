#' Export a plot or scene to an image file
#'
#' Render explicit graphical content to JPEG, PNG, SVG or WebP using an
#' independent graphics device. Export dimensions do not depend on the Plots
#' pane or on the size of the current device.
#'
#' @param plot An explicit ggplot, grid grob, `gc_element` or `gc_viewport`/scene.
#'   A filename, screenshot or resolved `gc_layout` is not an accepted source.
#' @param type Single case-insensitive format: `"jpg"`, `"jpeg"`, `"png"`,
#'   `"svg"` or `"webp"`. JPG and JPEG use the same encoder.
#' @param dir Destination directory, created recursively when necessary.
#'   Required: there is no default, so nothing is written unless the caller
#'   chooses a location, such as `tempdir()`. Do not include the filename here.
#' @param filename Basename without directory components. The selected extension
#'   is appended if absent. A supplied extension must match `type`, with JPG and
#'   JPEG treated as interchangeable.
#' @param width,height Positive numeric dimensions in logical pixels. Defaults
#'   are 800 by 600, independent of the scene's root reference dimensions.
#' @param dpi Positive raster density. Raster output is
#'   `round(width * dpi / 96)` by `round(height * dpi / 96)` pixels. SVG dimensions
#'   are `width / 96` by `height / 96` inches, independent of DPI.
#' @param background Single valid device background color. PNG, SVG and WebP
#'   allow `"transparent"`; JPEG requires an opaque color. Opaque scene
#'   backgrounds still cover the device background.
#' @param quality Number from zero to 100 for lossy JPEG/WebP encoding. It is
#'   validated but has no effect on PNG or SVG.
#' @param overwrite Logical; whether an existing file may be replaced. The
#'   default `FALSE` protects existing output.
#'
#' @section Optional codecs:
#' PNG and JPEG require **ragg**, SVG requires **svglite**, and WebP requires
#' **ragg** and **webp**. Missing packages raise `ggcarto_missing_dependency` with
#' installation guidance; no packages are installed automatically. SVG preserves
#' vector content, but source raster layers remain embedded raster images.
#'
#' @section Device and file safety:
#' Export opens its own device, draws with [gc_render()], closes that device and
#' restores the caller's device, including after rendering errors. It does not
#' depend on an implicit last plot and does not mutate scene declarations.
#'
#' A temporary file is written in the destination directory and renamed only
#' after successful rendering and encoding. A rendering failure leaves an
#' existing destination unchanged. Temporary files are removed; a newly created
#' directory may remain after an error. Concurrent writers to the same path
#' are not coordinated.
#'
#' @section Limits and conditions:
#' Raster output must have at least one pixel per axis and at most 100 million
#' pixels in total. WebP additionally limits each axis to 16383 pixels. Higher
#' DPI increases raster density without changing logical layout size. Font
#' metrics can differ between graphics devices.
#'
#' Invalid arguments raise `ggcarto_error`; an existing destination raises
#' `ggcarto_file_exists`; directory/publication failures raise `ggcarto_export`.
#' Layout warnings remain visible and normally do not prevent saving, whereas
#' invalid constraints still fail. PDF is not supported by `type`; use a PDF
#' device with [gc_render()] when that format is required.
#'
#' @returns The normalized absolute path of the exported file, invisibly.
#'   The output file and, if necessary, its parent directory are created as
#'   side effects.
#' @seealso [gc_render()], [gc_viewport()], [gc_resolve()]
#' @examples
#' scene <- gc_viewport(list(gc_place(gc_text("Map export"), left = 12, top = 12)))
#' if (requireNamespace("ragg", quietly = TRUE)) {
#'   file <- gc_save(scene,
#'     type = "png", dir = tempdir(),
#'     filename = basename(tempfile("ggcarto-")), width = 400, height = 200,
#'     dpi = 144
#'   )
#'   stopifnot(file.exists(file))
#'   unlink(file)
#' }
#' if (requireNamespace("svglite", quietly = TRUE)) {
#'   file <- gc_save(scene,
#'     type = "svg", dir = tempdir(),
#'     filename = basename(tempfile("ggcarto-")), width = 400, height = 200,
#'     background = "transparent"
#'   )
#'   stopifnot(file.exists(file))
#'   unlink(file)
#' }
#' @export
gc_save <- function(
  plot,
  type = "png",
  dir,
  filename = "plot",
  width = 800,
  height = 600,
  dpi = 96,
  background = "white",
  quality = 90,
  overwrite = FALSE
) {
  node <- as_l_node(plot)
  if (missing(dir)) {
    gc_abort(
      "dir must be supplied; for example, use dir = tempdir().",
      property = "dir"
    )
  }
  type <- export_type(type)
  filename <- export_filename(type, dir, filename)
  settings <- export_settings(
    type, width, height, dpi, background, quality, overwrite
  )
  require_export_codecs(type)
  destination <- export_destination(dir, filename, overwrite)
  temporary <- tempfile(
    pattern = ".ggcarto-",
    tmpdir = dirname(destination),
    fileext = paste0(".", type)
  )
  on.exit(unlink(temporary), add = TRUE)
  render_export(node, temporary, settings)
  publish_export(temporary, destination)
  invisible(destination)
}

export_type <- function(type) {
  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    gc_abort("type must be jpg, jpeg, png, svg or webp.", property = "type")
  }
  type <- tolower(type)
  if (!type %in% c("jpg", "jpeg", "png", "svg", "webp")) {
    gc_abort("type must be jpg, jpeg, png, svg or webp.", property = "type")
  }
  type
}

export_filename <- function(type, dir, filename) {
  for (property in c("dir", "filename")) {
    value <- if (property == "dir") dir else filename
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !nzchar(value)
    ) {
      gc_abort(
        paste0(property, " must be a nonempty string."),
        property = property
      )
    }
  }
  if (filename %in% c(".", "..") || grepl("[/\\\\]", filename)) {
    gc_abort(
      "filename must be a file name, without a directory path.",
      property = "filename"
    )
  }
  extension <- tolower(tools::file_ext(filename))
  aliases <- if (type %in% c("jpg", "jpeg")) c("jpg", "jpeg") else type
  if (nzchar(extension) && !extension %in% aliases) {
    gc_abort(
      paste0(
        "filename extension does not match type; ",
        "omit it or use the selected format."
      ),
      property = "filename"
    )
  }
  if (!nzchar(extension)) {
    filename <- paste0(filename, ".", type)
  }
  filename
}

export_settings <- function(
  type,
  width,
  height,
  dpi,
  background,
  quality,
  overwrite
) {
  width <- scalar_number(width, "width", TRUE)
  height <- scalar_number(height, "height", TRUE)
  dpi <- scalar_number(dpi, "dpi", TRUE)
  quality <- scalar_number(quality, "quality")
  if (quality < 0 || quality > 100) {
    gc_abort("quality must be between 0 and 100.", property = "quality")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    gc_abort("overwrite must be TRUE or FALSE.", property = "overwrite")
  }
  if (
    !is.character(background) || length(background) != 1L || is.na(background)
  ) {
    gc_abort("background must be a color string.", property = "background")
  }
  rgba <- tryCatch(
    grDevices::col2rgb(background, alpha = TRUE),
    error = function(condition) {
      gc_abort("background must be a valid color.", property = "background")
    }
  )
  if (type %in% c("jpg", "jpeg") && rgba[[4L]] != 255L) {
    gc_abort(
      "JPEG does not support transparency; choose an opaque background.",
      property = "background"
    )
  }
  pixel_width <- round(width * dpi / 96)
  pixel_height <- round(height * dpi / 96)
  if (
    type != "svg" &&
      (any(!is.finite(c(pixel_width, pixel_height))) ||
        min(pixel_width, pixel_height) < 1 ||
        pixel_width * pixel_height > 1e8)
  ) {
    gc_abort(
      paste0(
        "Raster dimensions must produce at least one pixel per axis ",
        "and at most 100 million pixels."
      ),
      property = "width/height"
    )
  }
  if (type == "webp" && max(pixel_width, pixel_height) > 16383) {
    gc_abort(
      "WebP supports at most 16383 pixels per axis.",
      property = "width/height"
    )
  }
  list(
    type = type,
    width = width,
    height = height,
    dpi = dpi,
    background = background,
    quality = quality,
    pixel_width = pixel_width,
    pixel_height = pixel_height
  )
}

require_export_codecs <- function(type) {
  packages <- if (type == "svg") {
    "svglite"
  } else if (type == "webp") {
    c("ragg", "webp")
  } else {
    "ragg"
  }
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      gc_abort(
        paste0(
          "Exporting ",
          type,
          " requires '",
          package,
          "'. Run install.packages('",
          package,
          "')."
        ),
        "missing_dependency"
      )
    }
  }
  invisible(NULL)
}

export_destination <- function(dir, filename, overwrite) {
  dir <- path.expand(dir)
  if (
    !dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ) {
    gc_abort("Cannot create destination directory.", "export", property = "dir")
  }
  destination <- file.path(
    normalizePath(dir, winslash = "/", mustWork = TRUE),
    filename
  )
  if (dir.exists(destination) || (file.exists(destination) && !overwrite)) {
    gc_abort(
      paste0(
        "Destination already exists; ",
        "choose another name or set overwrite = TRUE."
      ),
      "file_exists",
      property = "filename"
    )
  }
  destination
}

render_export <- function(node, temporary, settings) {
  caller <- grDevices::dev.cur()
  device <- NULL
  on.exit(
    {
      if (!is.null(device) && device %in% grDevices::dev.list()) {
        grDevices::dev.off(device)
      }
      if (
        caller != 1L &&
          caller %in% grDevices::dev.list() &&
          grDevices::dev.cur() != caller
      ) {
        grDevices::dev.set(caller)
      }
    },
    add = TRUE
  )
  capture <- NULL
  type <- settings$type
  if (type == "svg") {
    svglite::svglite(
      temporary,
      width = settings$width / 96,
      height = settings$height / 96,
      bg = settings$background
    )
  } else if (type == "png") {
    ragg::agg_png(
      temporary,
      width = settings$pixel_width,
      height = settings$pixel_height,
      res = settings$dpi,
      background = settings$background
    )
  } else if (type %in% c("jpg", "jpeg")) {
    ragg::agg_jpeg(
      temporary,
      width = settings$pixel_width,
      height = settings$pixel_height,
      res = settings$dpi,
      background = settings$background,
      quality = settings$quality
    )
  } else {
    capture <- ragg::agg_capture(
      width = settings$pixel_width,
      height = settings$pixel_height,
      res = settings$dpi,
      background = settings$background
    )
  }
  device <- grDevices::dev.cur()
  gc_render(
    node,
    width = settings$width,
    height = settings$height,
    dpi = settings$dpi
  )
  bitmap <- if (!is.null(capture)) capture() else NULL
  grDevices::dev.off(device)
  device <- NULL
  if (!is.null(bitmap)) {
    colors <- grDevices::col2rgb(as.vector(t(bitmap)), alpha = TRUE)
    pixels <- array(
      as.raw(colors),
      dim = c(4L, settings$pixel_width, settings$pixel_height)
    )
    webp::write_webp(pixels, target = temporary, quality = settings$quality)
  }
  invisible(NULL)
}

publish_export <- function(temporary, destination) {
  if (
    !file.exists(temporary) ||
      file.info(temporary)$size == 0 ||
      !file.rename(temporary, destination)
  ) {
    gc_abort("Could not publish the exported image.", "export", property = "dir")
  }
  invisible(destination)
}
