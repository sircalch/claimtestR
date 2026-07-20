#' Test whether an effect retains its direction
#'
#' @param x Numeric estimates from resampling or sensitivity analyses.
#' @param minimum_proportion Required proportion with the modal non-zero
#'   direction. Defaults to `0.95`.
#' @param na.rm Whether missing estimates should be removed.
#'
#' @return A `claim_test` object.
#' @export
expect_stable_direction <- function(x, minimum_proportion = 0.95, na.rm = FALSE) {
  if (!is.numeric(x) || length(x) < 2L) {
    stop("`x` must contain at least two numeric estimates.", call. = FALSE)
  }
  assert_number(minimum_proportion, "minimum_proportion", lower = 0)
  if (minimum_proportion > 1) stop("`minimum_proportion` must be <= 1.", call. = FALSE)
  if (anyNA(x)) {
    if (!isTRUE(na.rm)) stop("`x` contains missing values; use `na.rm = TRUE` to remove them.", call. = FALSE)
    x <- x[!is.na(x)]
  }
  if (!length(x)) stop("No estimates remain after removing missing values.", call. = FALSE)

  signs <- sign(x)
  counts <- table(factor(signs, levels = c(-1, 0, 1)))
  modal_direction <- c(-1, 0, 1)[which.max(counts)]
  proportion <- max(counts) / length(signs)
  direction_label <- c(`-1` = "negative", `0` = "zero", `1` = "positive")[[as.character(modal_direction)]]

  claim_result(
    proportion >= minimum_proportion && modal_direction != 0,
    claim = "effect direction is stable",
    observed = proportion,
    expected = paste0(">= ", minimum_proportion),
    details = list(direction = direction_label, counts = counts, n = length(signs)),
    call = match.call()
  )
}

#' Test whether a candidate model improves on a reference
#'
#' @param candidate Numeric metric for the candidate model.
#' @param reference Numeric metric for the reference model.
#' @param direction Whether smaller or larger metric values are better.
#' @param minimum Minimum required improvement, inclusive.
#'
#' @return A `claim_test` object.
#' @export
expect_model_improves <- function(candidate, reference,
                                  direction = c("lower", "higher"), minimum = 0) {
  assert_number(candidate, "candidate")
  assert_number(reference, "reference")
  assert_number(minimum, "minimum", lower = 0)
  direction <- match.arg(direction)
  improvement <- if (direction == "lower") reference - candidate else candidate - reference
  claim_result(
    improvement >= minimum && improvement > 0,
    claim = "candidate model improves on the reference",
    observed = improvement,
    expected = if (minimum == 0) "> 0" else paste0(">= ", minimum),
    details = list(candidate = candidate, reference = reference, direction = direction),
    call = match.call()
  )
}

