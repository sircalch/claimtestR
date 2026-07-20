#' Collect and summarize claim tests
#'
#' @param ... `claim_test` objects, or lists containing them.
#'
#' @return A `claim_test_summary` object.
#' @export
test_claims <- function(...) {
  inputs <- list(...)
  results <- do.call(c, lapply(inputs, function(x) {
    if (inherits(x, "claim_test")) list(x) else x
  }))
  if (!length(results)) stop("Provide at least one claim test.", call. = FALSE)
  valid <- vapply(results, inherits, logical(1), what = "claim_test")
  if (!all(valid)) stop("Every input must be a `claim_test` object.", call. = FALSE)
  passed <- vapply(results, `[[`, logical(1), "passed")
  structure(
    list(total = length(results), passed = sum(passed), failed = sum(!passed), results = results),
    class = "claim_test_summary"
  )
}

#' @export
print.claim_test_summary <- function(x, ...) {
  cat(sprintf("Claim tests: %d total | %d passed | %d failed\n", x$total, x$passed, x$failed))
  for (result in x$results) print(result)
  invisible(x)
}
