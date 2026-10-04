gc_abort <- function(
  message,
  subclass = "invalid",
  node = NULL,
  property = NULL
) {
  if (!is.null(node)) {
    message <- paste0("Node '", node, "': ", message)
  }
  stop(structure(
    list(
      message = message,
      call = NULL,
      node = node,
      property = property
    ),
    class = c(paste0("ggcarto_", subclass), "ggcarto_error", "error", "condition")
  ))
}

gc_warn <- function(message, subclass, node = NULL, property = NULL) {
  if (!is.null(node)) {
    message <- paste0("Node '", node, "': ", message)
  }
  warning(structure(
    list(
      message = message,
      call = NULL,
      node = node,
      property = property
    ),
    class = c(
      paste0("ggcarto_", subclass),
      "ggcarto_warning",
      "warning",
      "condition"
    )
  ))
}

scalar_number <- function(value, property, positive = FALSE) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      (positive && value <= 0)
  ) {
    gc_abort(
      paste0(
        property,
        " must be a finite ",
        if (positive) "positive " else "",
        "number."
      ),
      property = property
    )
  }
  as.numeric(value)
}

`%||%` <- function(value, fallback) if (is.null(value)) fallback else value
