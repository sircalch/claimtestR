# Internal extraction interface ------------------------------------------------

extract_claim_data <- function(x, ...) {
  UseMethod("extract_claim_data")
}

#' @exportS3Method extract_claim_data numeric
extract_claim_data.numeric <- function(x, term = NULL, estimate = NULL,
                                       conf.int = NULL, level = 0.95, ...) {
  validate_level(level)
  if (anyNA(x) || any(!is.finite(x))) {
    stop("`x` must not contain missing or non-finite values.", call. = FALSE)
  }
  if (length(x) == 1L) {
    value <- unname(x)
  } else if (!is.null(term) && !is.null(names(x)) && term %in% names(x)) {
    value <- unname(x[[term]])
  } else {
    stop("A numeric `x` must be scalar, or named with a matching `term`.", call. = FALSE)
  }
  new_claim_data(value, term = term, source = "numeric")
}

#' @exportS3Method extract_claim_data data.frame
extract_claim_data.data.frame <- function(x, term = NULL, estimate = NULL,
                                          conf.int = NULL, level = 0.95, ...) {
  validate_level(level)
  if (!nrow(x)) stop("`x` must contain at least one result row.", call. = FALSE)
  row <- select_term_row(x, term)
  estimate_col <- estimate %||% first_name(x, c("estimate", "Estimate", "effect"))
  validate_column_name(x, estimate_col, "estimate")

  if (is.null(conf.int)) {
    lower <- first_name(x, c("conf.low", "conf_low", "lower", "lwr"))
    upper <- first_name(x, c("conf.high", "conf_high", "upper", "upr"))
    interval_cols <- if (!is.null(lower) && !is.null(upper)) c(lower, upper) else NULL
  } else {
    if (!is.character(conf.int) || length(conf.int) != 2L || anyNA(conf.int)) {
      stop("`conf.int` must name exactly two interval columns.", call. = FALSE)
    }
    interval_cols <- conf.int
    validate_column_name(x, interval_cols[1L], "lower confidence interval")
    validate_column_name(x, interval_cols[2L], "upper confidence interval")
  }

  result <- new_claim_data(
    estimate = scalar_numeric(row[[estimate_col]], estimate_col),
    conf.low = if (is.null(interval_cols)) NULL else scalar_numeric(row[[interval_cols[1L]]], interval_cols[1L]),
    conf.high = if (is.null(interval_cols)) NULL else scalar_numeric(row[[interval_cols[2L]]], interval_cols[2L]),
    term = term %||% inferred_term(row),
    source = "data.frame"
  )
  validate_interval(result)
}

#' @exportS3Method extract_claim_data matrix
extract_claim_data.matrix <- function(x, term = NULL, estimate = NULL,
                                      conf.int = NULL, level = 0.95, ...) {
  data <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!is.null(rownames(x)) && !identical(rownames(x), as.character(seq_len(nrow(x))))) {
    data$term <- rownames(x)
  }
  extract_claim_data.data.frame(data, term, estimate, conf.int, level, ...)
}

#' @exportS3Method extract_claim_data lm
extract_claim_data.lm <- function(x, term = NULL, estimate = NULL,
                                  conf.int = NULL, level = 0.95, ...) {
  validate_level(level)
  coefficients <- stats::coef(x)
  selected <- select_coefficient(coefficients, term)
  coefficient_table <- summary(x)$coefficients
  standard_error <- coefficient_table[selected$name, "Std. Error"]
  critical <- stats::qt(1 - (1 - level) / 2, df = stats::df.residual(x))
  result <- new_claim_data(
    selected$value,
    selected$value - critical * standard_error,
    selected$value + critical * standard_error,
    selected$name,
    source = "lm"
  )
  validate_interval(result)
}

#' @exportS3Method extract_claim_data glm
extract_claim_data.glm <- function(x, term = NULL, estimate = NULL,
                                   conf.int = NULL, level = 0.95, ...) {
  validate_glm_convergence(x)
  validate_level(level)
  coefficients <- stats::coef(x)
  selected <- select_coefficient(coefficients, term)
  coefficient_table <- summary(x)$coefficients
  standard_error <- coefficient_table[selected$name, "Std. Error"]
  critical <- stats::qnorm(1 - (1 - level) / 2)
  result <- new_claim_data(
    selected$value,
    selected$value - critical * standard_error,
    selected$value + critical * standard_error,
    selected$name,
    source = "glm"
  )
  validate_interval(result)
}

validate_glm_convergence <- function(x) {
  if (!isTRUE(x$converged)) {
    stop(
      "Statistical claims cannot be evaluated reliably because the `glm` model did not converge. ",
      "Refit the model and verify convergence before calling claimtestR; ",
      "the package will not repair or reinterpret the model automatically.",
      call. = FALSE
    )
  }
  invisible(x)
}

#' @exportS3Method extract_claim_data default
extract_claim_data.default <- function(x, ...) {
  stop(
    "Unsupported `x` class: ", paste(class(x), collapse = "/"),
    ". Use a numeric value, matrix, data frame, lm, or glm object.",
    call. = FALSE
  )
}

extract_term <- function(x, term = NULL, estimate = NULL, conf.int = NULL,
                         level = 0.95) {
  extract_claim_data(x, term = term, estimate = estimate,
                     conf.int = conf.int, level = level)
}

new_claim_data <- function(estimate, conf.low = NULL, conf.high = NULL,
                           term = NULL, source = NULL) {
  structure(
    list(estimate = estimate, conf.low = conf.low, conf.high = conf.high,
         term = term, source = source),
    class = "claim_data"
  )
}

select_term_row <- function(x, term) {
  if (is.null(term)) {
    if (nrow(x) != 1L) stop("`term` is required when results contain multiple rows.", call. = FALSE)
    return(x[1L, , drop = FALSE])
  }
  term_col <- first_name(x, c("term", "parameter", "coefficient"))
  if (is.null(term_col)) stop("Could not find a term column.", call. = FALSE)
  matches <- which(!is.na(x[[term_col]]) & as.character(x[[term_col]]) == term)
  if (length(matches) != 1L) stop("`term` must match exactly one row: ", term, call. = FALSE)
  x[matches, , drop = FALSE]
}

select_coefficient <- function(x, term) {
  if (is.null(term)) {
    if (length(x) != 1L) stop("`term` is required when the model has multiple coefficients.", call. = FALSE)
    term <- names(x)[1L] %||% "estimate"
  }
  if (is.null(names(x)) || !term %in% names(x)) {
    stop("Term not found in model coefficients: ", term, call. = FALSE)
  }
  value <- unname(x[[term]])
  if (is.na(value) || !is.finite(value)) {
    stop("Coefficient `", term, "` is missing or non-finite.", call. = FALSE)
  }
  list(name = term, value = value)
}

inferred_term <- function(x) {
  term_col <- first_name(x, c("term", "parameter", "coefficient"))
  if (is.null(term_col)) NULL else as.character(x[[term_col]][1L])
}

first_name <- function(x, candidates) {
  found <- candidates[candidates %in% names(x)]
  if (length(found)) found[[1L]] else NULL
}

validate_column_name <- function(x, name, role) {
  if (is.null(name)) stop("Could not find an ", role, " column.", call. = FALSE)
  if (!is.character(name) || length(name) != 1L || is.na(name) || !name %in% names(x)) {
    stop("Unknown ", role, " column: `", paste(name, collapse = "`, `"), "`.", call. = FALSE)
  }
}

scalar_numeric <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop("Column `", name, "` must contain one finite numeric value.", call. = FALSE)
  }
  unname(x)
}

validate_interval <- function(x) {
  if (!is.null(x$conf.low) && !is.null(x$conf.high) && x$conf.low > x$conf.high) {
    stop("The lower confidence limit must not exceed the upper limit.", call. = FALSE)
  }
  x
}

validate_level <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0 || x >= 1) {
    stop("`level` must be one finite numeric value strictly between 0 and 1.", call. = FALSE)
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x
