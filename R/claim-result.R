#' Create a structured claim-test result
#'
#' `claim_result()` is the common result type returned by all claim tests. It
#' is useful when extending claimtestR with domain-specific assertions.
#'
#' @param passed A single non-missing logical value.
#' @param claim A short description of the claim being tested.
#' @param observed The observed value relevant to the claim.
#' @param expected A description or value representing the expectation.
#' @param details Optional named list with diagnostic information.
#' @param call The call that produced the result.
#'
#' @return An object of class `claim_test`.
#' @export
claim_result <- function(passed, claim, observed = NULL, expected = NULL,
                         details = list(), call = NULL) {
  if (!is.logical(passed) || length(passed) != 1L || is.na(passed)) {
    stop("`passed` must be one non-missing logical value.", call. = FALSE)
  }
  if (!is.character(claim) || length(claim) != 1L || is.na(claim) || !nzchar(claim)) {
    stop("`claim` must be one non-empty character value.", call. = FALSE)
  }
  if (!is.list(details)) {
    stop("`details` must be a list.", call. = FALSE)
  }

  structure(
    list(
      passed = passed,
      claim = claim,
      observed = observed,
      expected = expected,
      details = details,
      call = call
    ),
    class = "claim_test"
  )
}

#' @export
print.claim_test <- function(x, ...) {
  marker <- if (isTRUE(x$passed)) "PASS" else "FAIL"
  cat(sprintf("[%s] %s\n", marker, x$claim))
  if (!is.null(x$observed)) {
    cat("  Observed: ", format_value(x$observed), "\n", sep = "")
  }
  if (!is.null(x$expected)) {
    cat("  Expected: ", format_value(x$expected), "\n", sep = "")
  }
  invisible(x)
}

#' @export
summary.claim_test <- function(object, ...) {
  structure(
    list(total = 1L, passed = as.integer(object$passed),
         failed = as.integer(!object$passed), results = list(object)),
    class = "claim_test_summary"
  )
}

#' @export
as.data.frame.claim_test <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    passed = x$passed,
    claim = x$claim,
    observed = format_value(x$observed),
    expected = format_value(x$expected),
    stringsAsFactors = FALSE
  )
}

format_value <- function(x) {
  if (is.null(x)) return("")
  if (length(x) == 0L) return("<empty>")
  paste(format(x, trim = TRUE), collapse = ", ")
}

