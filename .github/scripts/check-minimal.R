local({
  source_dir <- normalizePath(Sys.getenv("GGCARTO_SOURCE", "."), mustWork = TRUE)
  output_dir <- Sys.getenv("GGCARTO_CHECK_OUTPUT", tempfile("ggcarto-check-"))
  mode <- Sys.getenv("GGCARTO_CHECK_MODE", "no-suggests")
  stopifnot(mode %in% c("no-suggests", "with-tests"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  original_dir <- getwd()
  on.exit(setwd(original_dir), add = TRUE)
  setwd(output_dir)
  options(
    repos = c(CRAN = "https://cloud.r-project.org"),
    timeout = 600,
    Ncpus = 2L
  )
  Sys.unsetenv("NOT_CRAN")
  Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "false")

  metadata <- read.dcf(file.path(source_dir, "DESCRIPTION"))
  dependency_names <- function(description, fields) {
    columns <- c("Package", fields)
    database <- matrix(
      NA_character_,
      nrow = 1L,
      ncol = length(columns),
      dimnames = list(description[1L, "Package"], columns)
    )
    database[1L, "Package"] <- description[1L, "Package"]
    present <- intersect(fields, colnames(description))
    database[1L, present] <- description[1L, present]
    tools::package_dependencies(
      rownames(database),
      db = database,
      which = fields,
      recursive = FALSE
    )[[1L]]
  }
  hard_fields <- c("Depends", "Imports", "LinkingTo")
  required <- dependency_names(metadata, hard_fields)
  bundled <- rownames(installed.packages(priority = c("base", "recommended")))
  required <- setdiff(required, bundled)
  ggplot_version <- Sys.getenv("GGCARTO_GGPLOT2_VERSION")
  if (nzchar(ggplot_version)) {
    archive <- tempfile(fileext = ".tar.gz")
    download.file(
      paste0(
        "https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_",
        ggplot_version,
        ".tar.gz"
      ),
      archive,
      mode = "wb"
    )
    unpacked <- tempfile("ggplot2-source-")
    dir.create(unpacked)
    untar(archive, exdir = unpacked)
    ggplot_metadata <- read.dcf(file.path(unpacked, "ggplot2", "DESCRIPTION"))
    ggplot_dependencies <- dependency_names(ggplot_metadata, hard_fields)
    ggplot_dependencies <- setdiff(
      ggplot_dependencies,
      bundled
    )
    install.packages(
      unique(c(setdiff(required, "ggplot2"), ggplot_dependencies)),
      dependencies = NA
    )
    install.packages(archive, repos = NULL, type = "source")
    stopifnot(as.character(packageVersion("ggplot2")) == ggplot_version)
  } else {
    install.packages(required, dependencies = NA)
  }
  if (mode == "with-tests") {
    install.packages("testthat", dependencies = NA)
    stopifnot(requireNamespace("testthat", quietly = TRUE))
  }

  suggested <- dependency_names(metadata, "Suggests")
  visible <- vapply(
    suggested,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
  print(data.frame(package = suggested, available = visible))
  if (mode == "no-suggests") {
    stopifnot(!any(visible))
  }
  print(sessionInfo())
  writeLines(capture.output(sessionInfo()), "session-info.txt")
  write.table(
    data.frame(package = suggested, available = visible),
    "suggests-availability.tsv",
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  r_binary <- file.path(R.home("bin"), "R")
  build_exit <- system2(r_binary, c("CMD", "build", shQuote(source_dir)))
  stopifnot(build_exit == 0L)
  tarball <- paste0(
    metadata[1L, "Package"],
    "_",
    metadata[1L, "Version"],
    ".tar.gz"
  )
  check_exit <- system2(
    r_binary,
    c("CMD", "check", "--as-cran", "--no-manual", shQuote(tarball))
  )
  check_dir <- paste0(metadata[1L, "Package"], ".Rcheck")
  check_log <- readLines(file.path(check_dir, "00check.log"), warn = FALSE)
  stopifnot(
    check_exit == 0L,
    !any(grepl("^Status:.*(ERROR|WARNING)", check_log))
  )

  .libPaths(c(normalizePath(check_dir), .libPaths()))
  library(ggcarto)
  scene <- gc_viewport(
    list(gc_place(gc_text("Dependency check"), left = 10, top = 10)),
    width = 400,
    height = 200
  )
  resolved <- gc_resolve(scene, width = 800, height = 400)
  stopifnot(
    inherits(resolved, "gc_layout"),
    identical(unname(resolved$root$box), c(0, 0, 800, 400))
  )
  pdf("core-smoke.pdf", width = 8, height = 4)
  device <- dev.cur()
  on.exit(if (device %in% dev.list()) dev.off(device), add = TRUE)
  gc_render(scene, width = 800, height = 400)
  dev.off(device)
  stopifnot(file.info("core-smoke.pdf")$size > 0)
  cat("PASS:", mode, "checks and core rendering smoke test.\n")
})
