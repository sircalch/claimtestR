#' Test the direction of an effect
#'
#' Tests whether an estimate is strictly positive or negative. `x` may be a
#' numeric scalar, a one-row result data frame, or a fitted model supported by
#' `stats::coef()`.
#'
#' These are descriptive claims about the point estimate only; they do not
#' account for uncertainty. For an inferential statement about direction, use
#' [expect_interval_excludes()] with `value = 0` or [expect_superior()].
#' `level` only affects intervals that are extracted and stored with the result;
#' it does not change the decision.
#'
#' @param x A numeric estimate, result data frame, or fitted model.
#' @param term Optional term name.
#' @param estimate Optional estimate-column name for data frames.
#' @param level Confidence level used when extracting intervals from models.
#'
#' @details A non-converged `glm` object is rejected with an error because its
#'   coefficients cannot support reliable claim evaluation. claimtestR does not
#'   attempt to repair or reinterpret the model.
#'
#' @return A `claim_test` object.
#' @examples
#' expect_positive_effect(2.5)
#'
#' fit <- lm(mpg ~ wt, data = mtcars)
#' expect_negative_effect(fit, term = "wt")
#' @export
expect_positive_effect <- function(x, term = NULL, estimate = NULL, level = 0.95) {
  result <- extract_term(x, term, estimate, level = level)
  value <- result$estimate
  claim_result(
    value > 0,
    claim = "positive_effect",
    observed = value,
    expected = "> 0",
    term = result$term %||% term,
    message = direction_message(result$term %||% term, "positive"),
    evidence = paste0("estimate = ", format_number(value)),
    call = match.call()
  )
}

#' @rdname expect_positive_effect
#' @export
expect_negative_effect <- function(x, term = NULL, estimate = NULL, level = 0.95) {
  result <- extract_term(x, term, estimate, level = level)
  value <- result$estimate
  claim_result(
    value < 0,
    claim = "negative_effect",
    observed = value,
    expected = "< 0",
    term = result$term %||% term,
    message = direction_message(result$term %||% term, "negative"),
    evidence = paste0("estimate = ", format_number(value)),
    call = match.call()
  )
}

term_message <- function(term, text) {
  if (is.null(term)) paste0("The effect ", text) else paste0("`", term, "` ", text)
}

direction_message <- function(term, direction) {
  if (is.null(term)) {
    paste0("The estimated effect is ", direction, ".")
  } else {
    paste0("`", term, "` has a ", direction, " effect.")
  }
}

format_number <- function(x, digits = 4L) format(signif(x, digits), trim = TRUE)
