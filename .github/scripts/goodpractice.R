local({
  source_dir <- normalizePath(
    Sys.getenv("GGCARTO_SOURCE", "."),
    mustWork = TRUE
  )
  output_dir <- Sys.getenv("GGCARTO_GP_OUTPUT", tempfile("ggcarto-gp-"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  stopifnot(requireNamespace("goodpractice", quietly = TRUE))
  exports <- getNamespaceExports("goodpractice")
  required_exports <- c(
    "gp",
    "all_checks",
    "checks",
    "failed_checks",
    "results"
  )
  missing_exports <- setdiff(required_exports, exports)
  if (length(missing_exports) > 0L) {
    stop(
      "Installed goodpractice lacks: ",
      paste(missing_exports, collapse = ", "),
      call. = FALSE
    )
  }

  cat("R:", R.version.string, "\n")
  cat("goodpractice:", format(utils::packageVersion("goodpractice")), "\n")
  cat("Source:", source_dir, "\n\n")

  result <- goodpractice::gp(source_dir)
  print(result)

  executed <- goodpractice::checks(result)
  check_results <- goodpractice::results(result)
  failed <- goodpractice::failed_checks(result)
  not_evaluated <- check_results$check[is.na(check_results$passed)]

  # Reported in the summary but not blocking: large refactors are tracked
  # separately and must not be forced through numeric thresholds.
  advisory <- intersect(
    failed,
    c("cyclocomp", "complexity_function_length")
  )
  # `%||%` is called as an infix operator (R >= 4.1 has no base version), which
  # the unused-internal check cannot see. Any other unused function blocks.
  false_positives <- list(complexity_unused_internal = "`%||%`")
  if ("failed_positions" %in% exports) {
    positions <- goodpractice::failed_positions(result)
    for (check in intersect(failed, names(false_positives))) {
      lines <- vapply(
        positions[[check]],
        function(position) trimws(paste0("", position$line)),
        character(1)
      )
      if (length(lines) && all(lines %in% false_positives[[check]])) {
        advisory <- c(advisory, check)
      }
    }
  }
  blocking <- setdiff(failed, advisory)

  utils::write.csv(
    check_results,
    file.path(output_dir, "goodpractice.csv"),
    row.names = FALSE
  )
  if ("export_json" %in% exports) {
    goodpractice::export_json(
      result,
      file.path(output_dir, "goodpractice.json")
    )
  }

  summary_lines <- c(
    "## goodpractice",
    "",
    paste0("- goodpractice ", format(utils::packageVersion("goodpractice"))),
    paste0("- Checks executed: ", length(executed)),
    paste0("- Passed: ", sum(check_results$passed %in% TRUE)),
    paste0("- Failed: ", length(blocking)),
    paste0("- Advisory: ", length(advisory)),
    paste0("- Not evaluated: ", length(not_evaluated)),
    ""
  )
  if (length(blocking) > 0L) {
    summary_lines <- c(
      summary_lines,
      "### Failed checks",
      "",
      paste0("- `", blocking, "`"),
      ""
    )
  }
  if (length(advisory) > 0L) {
    summary_lines <- c(
      summary_lines,
      "### Advisory checks",
      "",
      paste0("- `", advisory, "`"),
      ""
    )
  }
  if (length(not_evaluated) > 0L) {
    summary_lines <- c(
      summary_lines,
      "### Not evaluated",
      "",
      paste0("- `", not_evaluated, "`"),
      ""
    )
  }
  step_summary <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (nzchar(step_summary)) {
    cat(summary_lines, file = step_summary, sep = "\n", append = TRUE)
  }

  if (length(advisory) > 0L) {
    message("Advisory goodpractice checks: ", toString(advisory))
  }
  if (length(blocking) > 0L || length(not_evaluated) > 0L) {
    stop(
      length(blocking),
      " goodpractice check(s) failed and ",
      length(not_evaluated),
      " could not be evaluated.",
      call. = FALSE
    )
  }
})
