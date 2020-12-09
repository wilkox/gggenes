#' This function (suggested by Lionel Henry) allows vdiffr to be used
#' conditionally, so it can remain in 'Suggests'
#' @noRd
expect_doppelganger <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}
