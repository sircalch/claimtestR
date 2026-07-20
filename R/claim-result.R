#' Create a structured claim-test result
#'
#' `claim_result()` is the common result type returned by all claim tests. It
#' is useful when extending claimtestR with domain-specific assertions.
#'
#' @param passed A single non-missing logical value.
#' @param claim A stable, machine-readable claim identifier such as
#'   `"positive_effect"`.
#' @param observed The observed value relevant to the claim.
#' @param expected A description or value representing the expectation.
#' @param term Optional model term or parameter name.
#' @param message A human-readable description of the claim.
#' @param evidence A concise human-readable description of the evidence.
#' @param details Optional named list with diagnostic information.
#' @param call The call that produced the result.
#'
#' @return An object of class `claim_test`.
#' @examples
#' result <- claim_result(
#'   passed = TRUE,
#'   claim = "positive_effect",
#'   observed = 2.4,
#'   expected = "> 0",
#'   term = "treatment",
#'   message = "The treatment effect is positive."
#' )
#' result
#' @export
claim_result <- function(passed, claim, observed = NULL, expected = NULL,
                         term = NULL, message = NULL, evidence = NULL,
                         details = list(), call = NULL) {
  if (!is.logical(passed) || length(passed) != 1L || is.na(passed)) {
    stop("`passed` must be one non-missing logical value.", call. = FALSE)
  }
  if (!is.character(claim) || length(claim) != 1L || is.na(claim) ||
      !grepl("^[a-z][a-z0-9_]*$", claim)) {
    stop("`claim` must be one snake_case identifier.", call. = FALSE)
  }
  if (!is.null(term) && (!is.character(term) || length(term) != 1L ||
                         is.na(term) || !nzchar(term))) {
    stop("`term` must be NULL or one non-empty character value.", call. = FALSE)
  }
  if (is.null(message)) message <- humanize_claim(claim)
  if (!is.character(message) || length(message) != 1L || is.na(message) || !nzchar(message)) {
    stop("`message` must be one non-empty character value.", call. = FALSE)
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
      term = term,
      message = message,
      evidence = evidence %||% format_value(observed),
      details = details,
      call = call
    ),
    class = "claim_test"
  )
}

#' @export
print.claim_test <- function(x, ...) {
  marker <- if (isTRUE(x$passed)) "PASS" else "FAIL"
  cat("Claim: ", x$message, "\n", sep = "")
  cat("Status: ", marker, "\n", sep = "")
  if (!is.null(x$term)) {
    cat("Term: ", x$term, "\n", sep = "")
  }
  if (!is.null(x$observed)) {
    cat("  Observed: ", format_value(x$observed), "\n", sep = "")
  }
  if (!is.null(x$expected)) {
    cat("  Expected: ", format_value(x$expected), "\n", sep = "")
  }
  if (!is.null(x$evidence) && nzchar(x$evidence)) {
    cat("  Evidence: ", x$evidence, "\n", sep = "")
  }
  invisible(x)
}

#' @export
summary.claim_test <- function(object, ...) {
  new_claim_suite(list(object))
}

#' @export
as.data.frame.claim_test <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    passed = x$passed,
    claim = x$claim,
    term = x$term %||% "",
    status = if (x$passed) "PASS" else "FAIL",
    observed = format_value(x$observed),
    expected = format_value(x$expected),
    evidence = x$evidence %||% "",
    message = x$message,
    stringsAsFactors = FALSE
  )
}

humanize_claim <- function(x) {
  text <- gsub("_", " ", x, fixed = TRUE)
  paste0(toupper(substr(text, 1L, 1L)), substring(text, 2L), ".")
}

format_value <- function(x) {
  if (is.null(x)) return("")
  if (length(x) == 0L) return("<empty>")
  paste(format(x, trim = TRUE), collapse = ", ")
}
