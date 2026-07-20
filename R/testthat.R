#' Require a claim to pass in a testthat test
#'
#' Bridges the non-stopping `claim_test` result with `testthat`. A failed claim
#' becomes a failed test expectation, which makes it suitable for continuous
#' integration and regression tests of scientific conclusions.
#'
#' @param claim A `claim_test` object.
#'
#' @return The claim, invisibly.
#' @examples
#' if (requireNamespace("testthat", quietly = TRUE)) {
#'   expect_claim_passes(expect_positive_effect(2))
#' }
#' @export
expect_claim_passes <- function(claim) {
  if (!inherits(claim, "claim_test")) {
    stop("`claim` must be a `claim_test` object.", call. = FALSE)
  }
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("Package `testthat` is required by `expect_claim_passes()`.", call. = FALSE)
  }
  info <- paste0(claim$message, " Evidence: ", claim$evidence)
  testthat::expect_true(claim$passed, info = info)
  invisible(claim)
}
