#' Test superiority and non-inferiority with a confidence interval
#'
#' Interval-based inferential claims about an effect \eqn{\theta}, such as a
#' treatment-minus-control difference. Both assertions use only the interval,
#' never the point estimate, and both require an analyst-supplied margin.
#'
#' With `direction = "higher"` (larger values are better):
#' * superiority by margin \eqn{\delta \ge 0} passes when
#'   \eqn{\delta < L}, where \eqn{L} is the lower interval limit;
#' * non-inferiority with margin \eqn{\Delta > 0} passes when
#'   \eqn{-\Delta < L}.
#'
#' With `direction = "lower"` (smaller values are better), the conditions
#' mirror onto the upper limit \eqn{U}: \eqn{U < -\delta} and \eqn{U < \Delta}.
#'
#' Interval endpoints count as included, consistent with
#' [expect_interval_excludes()]. A claim therefore fails when the margin equals
#' the relevant endpoint.
#'
#' @section Relation to hypothesis tests:
#' Using a two-sided interval at `level = 1 - 2 * alpha` is equivalent to a
#' one-sided test at significance level `alpha`. For example, `level = 0.90`
#' corresponds to a one-sided test at 0.05. The default `level = 0.95`
#' corresponds to a one-sided test at 0.025, the convention in many clinical
#' guidelines. claimtestR never chooses the margin or the level for the analyst.
#'
#' @inheritParams expect_interval_excludes
#' @param margin For `expect_superior()`, the superiority margin
#'   \eqn{\delta \ge 0}; `0` tests plain superiority. For
#'   `expect_noninferior()`, the non-inferiority margin \eqn{\Delta > 0}.
#' @param direction `"higher"` when larger values of the effect are better, or
#'   `"lower"` when smaller values are better.
#'
#' @details A non-converged `glm` object is rejected with an error because its
#'   coefficients and intervals cannot support reliable claim evaluation.
#'
#' @return A `claim_test` object.
#' @examples
#' diff <- data.frame(term = "treatment", estimate = 1.4,
#'                    conf.low = 0.3, conf.high = 2.5)
#' expect_superior(diff, term = "treatment")
#' expect_superior(diff, term = "treatment", margin = 0.5)
#' expect_noninferior(diff, term = "treatment", margin = 1)
#'
#' # Smaller is better, e.g. a difference in adverse-event rates
#' ae <- data.frame(estimate = -0.02, conf.low = -0.05, conf.high = 0.01)
#' expect_noninferior(ae, margin = 0.03, direction = "lower")
#' @export
expect_superior <- function(x, margin = 0, term = NULL,
                            direction = c("higher", "lower"),
                            conf.int = NULL, estimate = NULL, level = 0.95) {
  assert_number(margin, "margin", lower = 0)
  direction <- match.arg(direction)
  result <- extract_term(x, term, estimate, conf.int, level)
  require_interval(result)
  if (direction == "higher") {
    passed <- margin < result$conf.low
    expected <- paste0("lower limit > ", margin)
  } else {
    passed <- result$conf.high < -margin
    expected <- paste0("upper limit < ", -margin)
  }
  selected_term <- result$term %||% term
  claim_result(
    passed,
    claim = "superior",
    observed = c(result$conf.low, result$conf.high),
    expected = expected,
    term = selected_term,
    message = term_message(selected_term, paste0(
      "is superior", if (margin > 0) paste0(" by margin ", margin) else "",
      " (", direction, " is better).")),
    evidence = interval_evidence(result, level),
    details = list(margin = margin, direction = direction, level = level),
    call = match.call()
  )
}

#' @rdname expect_superior
#' @export
expect_noninferior <- function(x, margin, term = NULL,
                               direction = c("higher", "lower"),
                               conf.int = NULL, estimate = NULL, level = 0.95) {
  assert_number(margin, "margin", lower = 0)
  if (margin == 0) {
    stop("`margin` must be > 0 for non-inferiority; use expect_superior() for a zero margin.",
         call. = FALSE)
  }
  direction <- match.arg(direction)
  result <- extract_term(x, term, estimate, conf.int, level)
  require_interval(result)
  if (direction == "higher") {
    passed <- -margin < result$conf.low
    expected <- paste0("lower limit > ", -margin)
  } else {
    passed <- result$conf.high < margin
    expected <- paste0("upper limit < ", margin)
  }
  selected_term <- result$term %||% term
  claim_result(
    passed,
    claim = "noninferior",
    observed = c(result$conf.low, result$conf.high),
    expected = expected,
    term = selected_term,
    message = term_message(selected_term, paste0(
      "is non-inferior within margin ", margin, " (", direction, " is better).")),
    evidence = interval_evidence(result, level),
    details = list(margin = margin, direction = direction, level = level),
    call = match.call()
  )
}
