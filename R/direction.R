#' Test the direction of an effect
#'
#' Tests whether an estimate is strictly positive or negative. `x` may be a
#' numeric scalar, a one-row result data frame, or a fitted model supported by
#' [stats::coef()].
#'
#' @param x A numeric estimate, result data frame, or fitted model.
#' @param term Optional term name.
#' @param estimate Optional estimate-column name for data frames.
#' @param level Confidence level used when extracting intervals from models.
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
