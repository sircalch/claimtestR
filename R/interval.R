#' Test whether an interval excludes or includes a value
#'
#' These assertions treat interval endpoints as included. Therefore, an
#' interval excludes `value` only when the value is strictly below its lower
#' limit or strictly above its upper limit.
#'
#' @param x A result data frame, matrix, or fitted model.
#' @param term Optional term name.
#' @param value Numeric value evaluated against the interval.
#' @param conf.int Optional two-element character vector naming the lower and
#'   upper interval columns.
#' @param estimate Optional estimate-column name for data frames.
#' @param level Confidence level used to calculate model intervals.
#'
#' @return A `claim_test` object.
#' @examples
#' results <- data.frame(
#'   term = "treatment", estimate = 1.2,
#'   conf.low = 0.4, conf.high = 2
#' )
#' expect_interval_excludes(results, term = "treatment", value = 0)
#' expect_interval_includes(results, term = "treatment", value = 1)
#' @export
expect_interval_excludes <- function(x, term = NULL, value = 0,
                                     conf.int = NULL, estimate = NULL,
                                     level = 0.95) {
  assert_number(value, "value")
  result <- extract_term(x, term, estimate, conf.int, level)
  require_interval(result)
  passed <- value < result$conf.low || value > result$conf.high
  selected_term <- result$term %||% term
  claim_result(
    passed,
    claim = "interval_excludes",
    observed = c(result$conf.low, result$conf.high),
    expected = paste0(value, " outside interval"),
    term = selected_term,
    message = term_message(selected_term, paste0("has an interval that excludes ", value, ".")),
    evidence = interval_evidence(result, level),
    details = list(value = value, level = level),
    call = match.call()
  )
}

#' @rdname expect_interval_excludes
#' @export
expect_interval_includes <- function(x, term = NULL, value = 0,
                                     conf.int = NULL, estimate = NULL,
                                     level = 0.95) {
  assert_number(value, "value")
  result <- extract_term(x, term, estimate, conf.int, level)
  require_interval(result)
  passed <- value >= result$conf.low && value <= result$conf.high
  selected_term <- result$term %||% term
  claim_result(
    passed,
    claim = "interval_includes",
    observed = c(result$conf.low, result$conf.high),
    expected = paste0(value, " inside interval"),
    term = selected_term,
    message = term_message(selected_term, paste0("has an interval that includes ", value, ".")),
    evidence = interval_evidence(result, level),
    details = list(value = value, level = level),
    call = match.call()
  )
}

#' Test whether an effect is practically meaningful
#'
#' @inheritParams expect_positive_effect
#' @param minimum Minimum effect magnitude, inclusive.
#' @param direction Direction in which the threshold must be reached. The
#'   default, `"absolute"`, ignores sign.
#'
#' @return A `claim_test` object.
#' @examples
#' expect_practical_effect(-6, minimum = 5)
#' expect_practical_effect(-6, minimum = 5, direction = "negative")
#' @export
expect_practical_effect <- function(x, minimum, term = NULL, estimate = NULL,
                                    direction = c("absolute", "positive", "negative"),
                                    level = 0.95) {
  assert_number(minimum, "minimum", lower = 0)
  direction <- match.arg(direction)
  result <- extract_term(x, term, estimate, level = level)
  value <- result$estimate
  observed <- switch(direction, absolute = abs(value), positive = value, negative = -value)
  selected_term <- result$term %||% term
  claim_result(
    observed >= minimum,
    claim = "practical_effect",
    observed = value,
    expected = practical_expectation(direction, minimum),
    term = selected_term,
    message = term_message(selected_term, "reaches the practical-effect threshold."),
    evidence = paste0("estimate = ", format_number(value)),
    details = list(minimum = minimum, direction = direction),
    call = match.call()
  )
}

#' Test statistical equivalence
#'
#' Uses the confidence-interval inclusion rule: equivalence passes only when
#' the entire interval lies within the equivalence bounds.
#'
#' @inheritParams expect_interval_excludes
#' @param bounds Two finite numeric equivalence bounds in increasing order.
#'
#' @return A `claim_test` object.
#' @examples
#' results <- data.frame(estimate = 0.05, conf.low = -0.1, conf.high = 0.2)
#' expect_equivalent(results, bounds = c(-0.25, 0.25))
#' @export
expect_equivalent <- function(x, bounds, term = NULL, conf.int = NULL,
                              estimate = NULL, level = 0.95) {
  if (!is.numeric(bounds) || length(bounds) != 2L || anyNA(bounds) ||
      any(!is.finite(bounds)) || bounds[1L] >= bounds[2L]) {
    stop("`bounds` must be two finite numeric values in increasing order.", call. = FALSE)
  }
  result <- extract_term(x, term, estimate, conf.int, level)
  require_interval(result)
  passed <- result$conf.low >= bounds[1L] && result$conf.high <= bounds[2L]
  selected_term <- result$term %||% term
  claim_result(
    passed,
    claim = "equivalent",
    observed = c(result$conf.low, result$conf.high),
    expected = bounds,
    term = selected_term,
    message = term_message(selected_term, "is within the equivalence bounds."),
    evidence = interval_evidence(result, level),
    details = list(bounds = bounds, level = level),
    call = match.call()
  )
}

require_interval <- function(x) {
  if (is.null(x$conf.low) || is.null(x$conf.high)) {
    stop("No confidence interval could be extracted.", call. = FALSE)
  }
}

interval_evidence <- function(x, level) {
  paste0(format_number(level * 100), "% CI = [", format_number(x$conf.low),
         ", ", format_number(x$conf.high), "]")
}

practical_expectation <- function(direction, minimum) {
  switch(
    direction,
    absolute = paste0("absolute estimate >= ", minimum),
    positive = paste0("estimate >= ", minimum),
    negative = paste0("estimate <= -", minimum)
  )
}

assert_number <- function(x, name, lower = -Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < lower) {
    stop("`", name, "` must be one finite numeric value",
         if (is.finite(lower)) paste0(" >= ", lower) else "", ".", call. = FALSE)
  }
}
