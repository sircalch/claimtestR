#' Test the direction of an effect
#'
#' Tests whether an estimate is strictly positive or negative. `x` may be a
#' numeric scalar, a one-row result data frame, or a fitted model supported by
#' [stats::coef()].
#'
#' @param x A numeric estimate, result data frame, or fitted model.
#' @param term Optional term name.
#' @param estimate Optional estimate-column name for data frames.
#'
#' @return A `claim_test` object.
#' @export
expect_positive_effect <- function(x, term = NULL, estimate = NULL) {
  value <- extract_term(x, term, estimate)$estimate
  claim_result(
    value > 0,
    claim = term_claim(term, "effect is positive"),
    observed = value,
    expected = "> 0",
    call = match.call()
  )
}

#' @rdname expect_positive_effect
#' @export
expect_negative_effect <- function(x, term = NULL, estimate = NULL) {
  value <- extract_term(x, term, estimate)$estimate
  claim_result(
    value < 0,
    claim = term_claim(term, "effect is negative"),
    observed = value,
    expected = "< 0",
    call = match.call()
  )
}

term_claim <- function(term, text) {
  if (is.null(term)) text else paste0("`", term, "`: ", text)
}

