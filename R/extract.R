extract_term <- function(x, term = NULL, estimate = NULL, conf.int = NULL) {
  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
    return(list(estimate = unname(x), conf.low = NULL, conf.high = NULL))
  }

  if (is.data.frame(x)) {
    row <- select_term_row(x, term)
    estimate_col <- estimate %||% first_name(x, c("estimate", "Estimate", "effect"))
    if (is.null(estimate_col)) {
      stop("Could not find an estimate column. Supply `estimate`.", call. = FALSE)
    }
    interval_cols <- conf.int %||% c(
      first_name(x, c("conf.low", "conf_low", "lower", "lwr")),
      first_name(x, c("conf.high", "conf_high", "upper", "upr"))
    )
    interval_cols <- interval_cols[!vapply(interval_cols, is.null, logical(1))]

    return(list(
      estimate = scalar_numeric(row[[estimate_col]], estimate_col),
      conf.low = if (length(interval_cols) == 2L) scalar_numeric(row[[interval_cols[1L]]], interval_cols[1L]) else NULL,
      conf.high = if (length(interval_cols) == 2L) scalar_numeric(row[[interval_cols[2L]]], interval_cols[2L]) else NULL
    ))
  }

  extracted <- try(stats::coef(x), silent = TRUE)
  if (!inherits(extracted, "try-error") && is.numeric(extracted)) {
    if (is.null(term)) {
      if (length(extracted) != 1L) {
        stop("`term` is required when the model has multiple coefficients.", call. = FALSE)
      }
      value <- unname(extracted)
    } else {
      if (!term %in% names(extracted)) stop("Term not found in model coefficients: ", term, call. = FALSE)
      value <- unname(extracted[[term]])
    }
    interval <- extract_model_interval(x, term)
    return(list(estimate = value, conf.low = interval[1L], conf.high = interval[2L]))
  }

  stop("`x` must be a numeric scalar, a result data frame, or a fitted model.", call. = FALSE)
}

select_term_row <- function(x, term) {
  if (is.null(term)) {
    if (nrow(x) != 1L) stop("`term` is required when results contain multiple rows.", call. = FALSE)
    return(x[1L, , drop = FALSE])
  }
  term_col <- first_name(x, c("term", "parameter", "coefficient"))
  if (is.null(term_col)) stop("Could not find a term column.", call. = FALSE)
  matches <- which(as.character(x[[term_col]]) == term)
  if (length(matches) != 1L) stop("`term` must match exactly one row: ", term, call. = FALSE)
  x[matches, , drop = FALSE]
}

extract_model_interval <- function(x, term) {
  interval <- try(stats::confint(x), silent = TRUE)
  if (inherits(interval, "try-error")) return(c(NULL, NULL))
  if (is.null(term)) {
    if (nrow(interval) != 1L) return(c(NULL, NULL))
    return(unname(interval[1L, 1:2]))
  }
  if (!term %in% rownames(interval)) return(c(NULL, NULL))
  unname(interval[term, 1:2])
}

first_name <- function(x, candidates) {
  found <- candidates[candidates %in% names(x)]
  if (length(found)) found[[1L]] else NULL
}

scalar_numeric <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    stop("Column `", name, "` must contain one non-missing numeric value.", call. = FALSE)
  }
  unname(x)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

