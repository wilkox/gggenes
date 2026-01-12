#' Infix %||% operator, from rlang
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Assert that an argument is a scalar grid::unit object
#' @noRd
assert_scalar_unit <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!inherits(x, "unit") || length(x) != 1) {
    cli::cli_abort(
      "{.arg {arg}} must be a scalar {.cls grid::unit} object",
      call = call
    )
  }
}

#' Assert that an argument is one of a set of choices
#' @noRd
assert_choice <- function(
  x,
  choices,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is.character(x) || length(x) != 1 || !x %in% choices) {
    cli::cli_abort(
      "{.arg {arg}} must be one of {.or {.val {choices}}}",
      call = call
    )
  }
}

#' Assert that an argument is a scalar logical value
#' @noRd
assert_flag <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a scalar logical value",
      call = call
    )
  }
}

#' Assert that an argument is a scalar non-negative number
#' @noRd
assert_scalar_nonnegative_number <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0) {
    cli::cli_abort(
      "{.arg {arg}} must be a scalar non-negative number",
      call = call
    )
  }
}

#' Assert that an argument is a data frame
#' @noRd
assert_data_frame <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is.data.frame(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a data frame",
      call = call
    )
  }
}

#' Assert that an argument is a scalar character vector
#' @noRd
assert_scalar_character <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a scalar character vector",
      call = call
    )
  }
}

#' Assert that an argument is a ggplot2 aesthetic mapping
#' @noRd
assert_mapping <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!inherits(x, "uneval")) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls ggplot2::aes} mapping object",
      call = call
    )
  }
}

#' Check if a value is between two bounds (inclusive)
#' @noRd
between <- function(i, a, b) {
  i >= min(c(a, b)) & i <= max(c(a, b))
}
