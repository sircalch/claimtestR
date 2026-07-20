#' Test whether an effect retains its direction
#'
#' Numeric input is interpreted as estimates from existing resamples or
#' sensitivity analyses. For `lm` and `glm` objects, the function can generate
#' estimates using ordinary row bootstrap or leave-one-out refitting.
#'
#' @param x Numeric estimates, or an `lm`/`glm` model.
#' @param minimum_proportion Required proportion retaining the target direction.
#' @param na.rm Whether failed or missing resample estimates should be removed.
#' @param term Model term to evaluate.
#' @param method One of `"auto"`, `"estimates"`, `"bootstrap"`, or
#'   `"leave_one_out"`. Auto uses existing estimates for numeric input and
#'   bootstrap for supported models.
#' @param iterations Number of bootstrap iterations.
#' @param direction Target direction. Auto uses the modal non-zero direction for
#'   numeric input and the original coefficient direction for a model.
#' @param seed Optional integer seed for bootstrap reproducibility. The caller's
#'   random-number state is restored on exit.
#'
#' @return A `claim_test` object.
#' @examples
#' estimates <- c(rep(1.2, 19), -0.4)
#' expect_stable_direction(estimates, minimum_proportion = 0.95)
#'
#' fit <- lm(mpg ~ wt, data = mtcars)
#' expect_stable_direction(
#'   fit, term = "wt", method = "bootstrap", iterations = 20, seed = 1,
#'   minimum_proportion = 0.8
#' )
#' @export
expect_stable_direction <- function(
    x, minimum_proportion = 0.95, na.rm = FALSE, term = NULL,
    method = c("auto", "estimates", "bootstrap", "leave_one_out"),
    iterations = 1000L, direction = c("auto", "positive", "negative"),
    seed = NULL) {
  assert_number(minimum_proportion, "minimum_proportion", lower = 0)
  if (minimum_proportion > 1) stop("`minimum_proportion` must be <= 1.", call. = FALSE)
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }
  method <- match.arg(method)
  direction <- match.arg(direction)

  if (is.numeric(x)) {
    if (method == "auto") method <- "estimates"
    if (method != "estimates") {
      stop("Numeric `x` requires `method = \"estimates\"`.", call. = FALSE)
    }
    estimates <- validate_estimates(x, na.rm)
    reference <- resolve_numeric_direction(estimates, direction)
    selected_term <- term
  } else if (inherits(x, c("lm", "glm"))) {
    if (method == "auto") method <- "bootstrap"
    if (method == "estimates") {
      stop("Model input requires `method = \"bootstrap\"` or `\"leave_one_out\"`.", call. = FALSE)
    }
    original <- extract_claim_data(x, term = term)
    selected_term <- original$term
    reference <- resolve_model_direction(original$estimate, direction)
    estimates <- resample_model_estimates(x, selected_term, method, iterations, seed)
    estimates <- validate_estimates(estimates, na.rm)
  } else {
    stop("`x` must be numeric or an lm/glm model.", call. = FALSE)
  }

  target_sign <- if (is.null(reference)) {
    NA_real_
  } else {
    switch(reference, positive = 1, negative = -1, NA_real_)
  }
  proportion <- if (is.na(target_sign)) 0 else mean(sign(estimates) == target_sign)
  passed <- !is.na(target_sign) && proportion >= minimum_proportion
  evidence <- paste0(format_number(100 * proportion), "% of estimates were ",
                     reference %||% "without a unique direction")

  claim_result(
    passed,
    claim = "stable_direction",
    observed = proportion,
    expected = paste0(">= ", minimum_proportion),
    term = selected_term,
    message = term_message(selected_term, "retains its effect direction."),
    evidence = evidence,
    details = list(
      direction = reference, method = method, estimates = estimates,
      n = length(estimates), minimum_proportion = minimum_proportion
    ),
    call = match.call()
  )
}

validate_estimates <- function(x, na.rm) {
  if (!is.numeric(x) || length(x) < 2L) {
    stop("At least two numeric estimates are required.", call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x))) {
    if (!na.rm) {
      stop("Estimates contain missing or non-finite values; use `na.rm = TRUE` to remove them.", call. = FALSE)
    }
    x <- x[!is.na(x) & is.finite(x)]
  }
  if (length(x) < 2L) stop("Fewer than two valid estimates remain.", call. = FALSE)
  x
}

resolve_numeric_direction <- function(x, direction) {
  if (direction != "auto") return(direction)
  counts <- c(negative = sum(x < 0), positive = sum(x > 0))
  if (!length(counts) || max(counts) == 0 || sum(counts == max(counts)) != 1L) return(NULL)
  names(which.max(counts))
}

resolve_model_direction <- function(x, direction) {
  if (direction != "auto") return(direction)
  if (x > 0) "positive" else if (x < 0) "negative" else NULL
}

resample_model_estimates <- function(model, term, method, iterations, seed) {
  data <- stats::model.frame(model)
  n <- nrow(data)
  if (n < 3L) stop("Model resampling requires at least three observations.", call. = FALSE)
  if (method == "bootstrap") {
    if (!is.numeric(iterations) || length(iterations) != 1L || is.na(iterations) ||
        !is.finite(iterations) || iterations < 2 || iterations != as.integer(iterations)) {
      stop("`iterations` must be one integer >= 2.", call. = FALSE)
    }
    validate_seed(seed)
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit({
      if (old_seed_exists) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    if (!is.null(seed)) set.seed(seed)
    indices <- replicate(as.integer(iterations), sample.int(n, n, replace = TRUE), simplify = FALSE)
  } else {
    indices <- lapply(seq_len(n), function(i) setdiff(seq_len(n), i))
  }

  vapply(indices, function(index) {
    refit <- try(stats::update(model, data = data[index, , drop = FALSE]), silent = TRUE)
    if (inherits(refit, "try-error")) return(NA_real_)
    coefficient <- stats::coef(refit)
    if (is.null(names(coefficient)) || !term %in% names(coefficient)) return(NA_real_)
    unname(coefficient[[term]])
  }, numeric(1))
}

validate_seed <- function(seed) {
  if (is.null(seed)) return(invisible(NULL))
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed != as.integer(seed)) {
    stop("`seed` must be NULL or one finite integer.", call. = FALSE)
  }
  invisible(NULL)
}

#' Test whether a candidate model improves on a reference
#'
#' @param candidate Numeric metric for the candidate model.
#' @param reference Numeric metric for the reference model.
#' @param direction Whether smaller or larger metric values are better.
#' @param minimum Minimum required improvement, inclusive.
#'
#' @return A `claim_test` object.
#' @examples
#' expect_model_improves(candidate = 8, reference = 10, direction = "lower")
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
    claim = "model_improves",
    observed = improvement,
    expected = if (minimum == 0) "> 0" else paste0(">= ", minimum),
    message = "The candidate model improves on the reference model.",
    evidence = paste0("improvement = ", format_number(improvement)),
    details = list(candidate = candidate, reference = reference, direction = direction,
                   minimum = minimum),
    call = match.call()
  )
}
