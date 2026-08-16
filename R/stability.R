#' Test whether an effect retains its direction
#'
#' Numeric input is interpreted as estimates from existing resamples or
#' sensitivity analyses. For `lm` and `glm` objects, the function can generate
#' estimates using ordinary row bootstrap or leave-one-out refitting.
#'
#' @param x Numeric estimates, or an `lm`/`glm` model.
#' @param minimum_proportion Required proportion retaining the target direction.
#' @param na.rm For numeric input, whether missing or non-finite estimates should
#'   be removed. Model-refit failures are never hidden: they are recorded and
#'   count against the attempted-resample denominator.
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
#' @details For model resampling, at least 80% of attempted refits and at least
#'   two refits must succeed before the stability assessment is considered
#'   valid. Failed refits count against the observed stability proportion.
#'   Counts, error classes, messages, and captured warnings are retained in the
#'   returned object's `details` field. An insufficient success proportion
#'   returns a failed `claim_test` rather than silently dropping refits.
#'
#' Model refits recover the original data named in the `lm` or `glm` call and
#' align them to the observations retained by the fitted model. This allows
#' transformations in the response or predictors to be reevaluated for each
#' bootstrap or leave-one-out sample while preserving the fitted model's
#' `subset`, missing-value exclusions, weights, and call-level offset. The
#' stored formula object, including its environment, is used for every refit.
#'
#' For safety in version 0.1.2, the model's `data` argument must be a simple
#' name that resolves to an ordinary object with class exactly `data.frame`.
#' Columns must be unclassed atomic vectors or base factors. Active bindings,
#' additional data or column classes, and every data-producing call -- including
#' `subset()`, `transform()`, `within()`, and `get()` -- are rejected. Materialize
#' such a call first (for example, `d <- subset(source, keep)`) and fit with
#' `data = d`. Models fit with `model = FALSE` cannot be resampled because they
#' do not retain the model frame needed to identify the observations actually
#' used.
#'
#' Non-converged `glm` objects are rejected because their coefficients cannot
#' support reliable claim evaluation. Bootstrap refits that do not converge are
#' recorded as failed refits.
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
  resampling <- NULL

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
    resampling <- resample_model_estimates(x, selected_term, method, iterations, seed)
    estimates <- resampling$estimates[resampling$successful_refit]
  } else {
    stop("`x` must be numeric or an lm/glm model.", call. = FALSE)
  }

  target_sign <- if (is.null(reference)) {
    NA_real_
  } else {
    switch(reference, positive = 1, negative = -1, NA_real_)
  }
  if (is.null(resampling)) {
    proportion <- if (is.na(target_sign)) 0 else mean(sign(estimates) == target_sign)
    assessment_valid <- TRUE
    evidence <- paste0(format_number(100 * proportion), "% of estimates were ",
                       reference %||% "without a unique direction")
    details <- list(
      direction = reference, method = method, estimates = estimates,
      n = length(estimates), minimum_proportion = minimum_proportion
    )
  } else {
    direction_matches <- if (is.na(target_sign)) {
      0L
    } else {
      sum(sign(estimates) == target_sign)
    }
    proportion <- direction_matches / resampling$attempted
    conditional_proportion <- if (resampling$successful > 0L) {
      direction_matches / resampling$successful
    } else {
      0
    }
    assessment_valid <- resampling$successful >= 2L &&
      resampling$success_proportion >= minimum_resample_success()

    if (assessment_valid) {
      evidence <- paste0(
        format_number(100 * proportion), "% of attempted refits retained the ",
        reference %||% "target", " direction; ", resampling$successful, " of ",
        resampling$attempted, " refits succeeded (",
        format_number(100 * resampling$success_proportion), "%)."
      )
    } else {
      evidence <- paste0(
        "Stability could not be evaluated reliably: ", resampling$successful,
        " of ", resampling$attempted, " refits succeeded (",
        format_number(100 * resampling$success_proportion),
        "%). At least two successful refits and an 80% success proportion are required."
      )
    }

    details <- list(
      direction = reference,
      method = method,
      estimates = resampling$estimates,
      n = resampling$attempted,
      minimum_proportion = minimum_proportion,
      attempted = resampling$attempted,
      successful = resampling$successful,
      failed = resampling$failed,
      success_proportion = resampling$success_proportion,
      minimum_success_proportion = minimum_resample_success(),
      resampling_valid = assessment_valid,
      direction_matches = direction_matches,
      conditional_direction_proportion = conditional_proportion,
      failures = resampling$failures,
      warnings = resampling$warnings
    )
  }

  passed <- assessment_valid && !is.na(target_sign) &&
    proportion >= minimum_proportion
  claim_message <- if (!assessment_valid) {
    term_message(selected_term, "has insufficient valid refits to assess direction stability.")
  } else {
    term_message(selected_term, "retains its effect direction.")
  }

  claim_result(
    passed,
    claim = "stable_direction",
    observed = proportion,
    expected = paste0(">= ", minimum_proportion),
    term = selected_term,
    message = claim_message,
    evidence = evidence,
    details = details,
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
  prepared <- prepare_model_resampling(model)
  n <- data_frame_nrow(prepared$data)
  if (n < 3L) stop("Model resampling requires at least three observations.", call. = FALSE)
  if (method == "bootstrap") {
    if (!is.numeric(iterations) || length(iterations) != 1L || is.na(iterations) ||
        !is.finite(iterations) || iterations < 2 || iterations != as.integer(iterations)) {
      stop("`iterations` must be one integer >= 2.", call. = FALSE)
    }
    return(with_preserved_seed(seed, {
      indices <- replicate(
        as.integer(iterations), sample.int(n, n, replace = TRUE),
        simplify = FALSE
      )
      evaluate_resample_indices(model, prepared, indices, term)
    }))
  } else {
    indices <- lapply(seq_len(n), function(i) setdiff(seq_len(n), i))
  }

  evaluate_resample_indices(model, prepared, indices, term)
}

evaluate_resample_indices <- function(model, prepared, indices, term) {
  outcomes <- lapply(seq_along(indices), function(i) {
    index <- indices[[i]]
    refit_model_once(
      model,
      prepared$data[index, , drop = FALSE],
      term,
      weights = if (is.null(prepared$weights)) NULL else prepared$weights[index],
      offset = if (is.null(prepared$offset)) NULL else prepared$offset[index]
    )
  })

  successful_refit <- vapply(outcomes, `[[`, logical(1), "success")
  estimates <- vapply(outcomes, `[[`, numeric(1), "estimate")
  attempted <- length(outcomes)
  successful <- sum(successful_refit)
  failures <- collect_refit_failures(outcomes)
  warnings <- collect_refit_warnings(outcomes)

  structure(
    list(
      estimates = estimates,
      successful_refit = successful_refit,
      attempted = attempted,
      successful = successful,
      failed = attempted - successful,
      success_proportion = successful / attempted,
      failures = failures,
      warnings = warnings
    ),
    class = "claim_resample_result"
  )
}

refit_model_once <- function(model, data, term, weights = NULL, offset = NULL) {
  captured_warnings <- list()
  refit <- tryCatch(
    withCallingHandlers(
      update_model_for_resample(model, data, weights, offset),
      warning = function(w) {
        captured_warnings[[length(captured_warnings) + 1L]] <<- list(
          class = class(w)[1L],
          message = conditionMessage(w)
        )
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(refit, "error")) {
    return(refit_outcome(
      success = FALSE,
      failure_class = class(refit)[1L],
      failure_message = conditionMessage(refit),
      warnings = captured_warnings
    ))
  }

  assess_refit(refit, term, captured_warnings)
}

assess_refit <- function(refit, term, warnings = list()) {
  if (inherits(refit, "glm") && !isTRUE(refit$converged)) {
    return(refit_outcome(
      success = FALSE,
      failure_class = "glm_non_convergence",
      failure_message = "The refitted glm model did not converge.",
      warnings = warnings
    ))
  }

  coefficient <- tryCatch(stats::coef(refit), error = function(e) e)
  if (inherits(coefficient, "error")) {
    return(refit_outcome(
      success = FALSE,
      failure_class = class(coefficient)[1L],
      failure_message = conditionMessage(coefficient),
      warnings = warnings
    ))
  }
  if (is.null(names(coefficient)) || !term %in% names(coefficient)) {
    return(refit_outcome(
      success = FALSE,
      failure_class = "term_not_found",
      failure_message = paste0("Term `", term, "` was absent after refitting."),
      warnings = warnings
    ))
  }

  estimate <- unname(coefficient[[term]])
  if (!is.numeric(estimate) || length(estimate) != 1L || is.na(estimate) ||
      !is.finite(estimate)) {
    return(refit_outcome(
      success = FALSE,
      failure_class = "non_finite_coefficient",
      failure_message = paste0(
        "Term `", term, "` was missing or non-finite after refitting."
      ),
      warnings = warnings
    ))
  }

  refit_outcome(success = TRUE, estimate = estimate, warnings = warnings)
}

refit_outcome <- function(success, estimate = NA_real_, failure_class = NULL,
                          failure_message = NULL, warnings = list()) {
  list(
    success = success,
    estimate = estimate,
    failure_class = failure_class,
    failure_message = failure_message,
    warnings = warnings
  )
}

collect_refit_failures <- function(outcomes) {
  failed <- which(!vapply(outcomes, `[[`, logical(1), "success"))
  if (!length(failed)) return(empty_diagnostic_table())
  data.frame(
    attempt = failed,
    class = vapply(outcomes[failed], `[[`, character(1), "failure_class"),
    message = vapply(outcomes[failed], `[[`, character(1), "failure_message"),
    stringsAsFactors = FALSE
  )
}

collect_refit_warnings <- function(outcomes) {
  records <- lapply(seq_along(outcomes), function(i) {
    warnings <- outcomes[[i]]$warnings
    if (!length(warnings)) return(NULL)
    data.frame(
      attempt = rep.int(i, length(warnings)),
      class = vapply(warnings, `[[`, character(1), "class"),
      message = vapply(warnings, `[[`, character(1), "message"),
      stringsAsFactors = FALSE
    )
  })
  records <- Filter(Negate(is.null), records)
  if (!length(records)) return(empty_diagnostic_table())
  do.call(rbind, records)
}

empty_diagnostic_table <- function() {
  data.frame(
    attempt = integer(), class = character(), message = character(),
    stringsAsFactors = FALSE
  )
}

minimum_resample_success <- function() 0.8

with_preserved_seed <- function(seed, code) {
  validate_seed(seed)
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  if (!is.null(seed)) set.seed(seed)
  force(code)
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
