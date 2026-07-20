#' Test whether an interval excludes a value
#'
#' @param x A result data frame or fitted model.
#' @param term Optional term name.
#' @param value Numeric value that must be outside the interval.
#' @param conf.int Optional two-element character vector naming the lower and
#'   upper interval columns.
#' @param estimate Optional estimate-column name for data frames.
#'
#' @return A `claim_test` object.
#' @export
expect_interval_excludes <- function(x, term = NULL, value = 0,
                                     conf.int = NULL, estimate = NULL) {
  assert_number(value, "value")
  result <- extract_term(x, term, estimate, conf.int)
  if (is.null(result$conf.low) || is.null(result$conf.high)) {
    stop("No confidence interval could be extracted.", call. = FALSE)
  }
  passed <- value < result$conf.low || value > result$conf.high
  claim_result(
    passed,
    claim = term_claim(term, paste0("interval excludes ", value)),
    observed = c(result$conf.low, result$conf.high),
    expected = paste0(value, " outside interval"),
    details = list(value = value),
    call = match.call()
  )
}

#' Test whether an effect is practically meaningful
#'
#' @inheritParams expect_positive_effect
#' @param minimum Minimum absolute effect size, inclusive.
#'
#' @return A `claim_test` object.
#' @export
expect_practical_effect <- function(x, minimum, term = NULL, estimate = NULL) {
  assert_number(minimum, "minimum", lower = 0)
  value <- extract_term(x, term, estimate)$estimate
  claim_result(
    abs(value) >= minimum,
    claim = term_claim(term, "effect reaches the practical threshold"),
    observed = abs(value),
    expected = paste0(">= ", minimum),
    details = list(estimate = value, minimum = minimum),
    call = match.call()
  )
}

#' Test statistical equivalence
#'
#' Uses the confidence-interval inclusion rule: equivalence passes only when
#' the entire interval lies inside the equivalence bounds.
#'
#' @inheritParams expect_interval_excludes
#' @param bounds Two finite numeric equivalence bounds in increasing order.
#'
#' @return A `claim_test` object.
#' @export
expect_equivalent <- function(x, bounds, term = NULL, conf.int = NULL,
                              estimate = NULL) {
  if (!is.numeric(bounds) || length(bounds) != 2L || anyNA(bounds) ||
      any(!is.finite(bounds)) || bounds[1L] >= bounds[2L]) {
    stop("`bounds` must be two finite numeric values in increasing order.", call. = FALSE)
  }
  result <- extract_term(x, term, estimate, conf.int)
  if (is.null(result$conf.low) || is.null(result$conf.high)) {
    stop("No confidence interval could be extracted.", call. = FALSE)
  }
  passed <- result$conf.low > bounds[1L] && result$conf.high < bounds[2L]
  claim_result(
    passed,
    claim = term_claim(term, "effect is within equivalence bounds"),
    observed = c(result$conf.low, result$conf.high),
    expected = bounds,
    details = list(bounds = bounds),
    call = match.call()
  )
}

assert_number <- function(x, name, lower = -Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < lower) {
    stop("`", name, "` must be one finite numeric value",
         if (is.finite(lower)) paste0(" >= ", lower) else "", ".", call. = FALSE)
  }
}

