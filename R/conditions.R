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
    class = c(
      paste0("ggcarto_", subclass),
      "ggcarto_error",
      "error",
      "condition"
    )
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

is_finite_number <- function(value) {
  is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value)
}

is_single_string <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value)
}

is_nonempty_string <- function(value) {
  is_single_string(value) && nzchar(value)
}

is_flag <- function(value) {
  is.logical(value) && length(value) == 1L && !is.na(value)
}

is_choice <- function(value, choices) {
  length(value) == 1L && !is.na(value) && value %in% choices
}

has_unique_names <- function(values) {
  !is.null(names(values)) &&
    all(nzchar(names(values))) &&
    !anyDuplicated(names(values))
}

is_colour_value <- function(value) {
  length(value) == 1L &&
    (is.character(value) || is.numeric(value) || identical(value, NA))
}

colour_parses <- function(value) {
  tryCatch(
    {
      grDevices::col2rgb(value)
      TRUE
    },
    error = function(error) FALSE
  )
}

scalar_number <- function(value, property, positive = FALSE) {
  if (!is_finite_number(value) || (positive && value <= 0)) {
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

# `%||%` is only ever called as an infix operator, which static analysis does
# not see as a call; the named helper keeps it visible as used code. Base R
# provides an equivalent operator only from R 4.4.0.
null_default <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

`%||%` <- null_default
