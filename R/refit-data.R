prepare_model_resampling <- function(model) {
  model_frame <- model$model
  if (!is.data.frame(model_frame)) {
    stop(
      "Model resampling requires an lm or glm fitted with `model = TRUE` ",
      "so the observations actually used can be recovered safely.",
      call. = FALSE
    )
  }
  model_frame <- as_base_data_frame(model_frame)
  recovered <- recover_original_model_data(model, model_frame)
  data <- align_recovered_model_data(recovered, model_frame)
  validate_recovered_model_data(model, data, model_frame)

  weights <- NULL
  if (has_model_call_argument(model, "weights")) {
    weights <- stats::model.weights(model_frame)
    if (is.null(weights) || length(weights) != nrow(model_frame)) {
      stop(
        "Could not recover the weights used by the fitted model for refitting.",
        call. = FALSE
      )
    }
  }

  offset <- NULL
  if (has_model_call_argument(model, "offset")) {
    if (!"(offset)" %in% names(model_frame)) {
      stop(
        "Could not recover the call-level offset used by the fitted model for refitting.",
        call. = FALSE
      )
    }
    offset <- model_frame[["(offset)"]]
  }

  list(data = data, weights = weights, offset = offset)
}

recover_original_model_data <- function(model, model_frame) {
  data_expression <- model$call$data
  if (is.null(data_expression)) {
    data <- as_base_data_frame(model_frame)
  } else if (!is.name(data_expression) && !is_safe_subset_call(data_expression) &&
             model_frame_contains_original_variables(model, model_frame)) {
    data <- as_base_data_frame(model_frame)
  } else {
    formula_environment <- environment(stats::formula(model))
    if (is.null(formula_environment)) {
      stop(
        "Could not safely recover the original model data because the model formula has no environment.",
        call. = FALSE
      )
    }
    data <- evaluate_model_data_expression(data_expression, formula_environment)
    data <- as_base_data_frame(data)
  }

  required <- all.vars(stats::formula(model))
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      "Could not recover the original model data columns required for refitting: ",
      paste(missing, collapse = ", "),
      ". Fit the model with a named data frame containing the original variables.",
      call. = FALSE
    )
  }
  data
}

model_frame_contains_original_variables <- function(model, model_frame) {
  all(all.vars(stats::formula(model)) %in% names(model_frame))
}

evaluate_model_data_expression <- function(expression, environment) {
  if (is.name(expression)) {
    name <- as.character(expression)
    if (!exists(name, envir = environment, inherits = TRUE)) {
      stop(
        "Could not recover the original model data object `", name,
        "` from the model formula environment.",
        call. = FALSE
      )
    }
    return(get(name, envir = environment, inherits = TRUE))
  }

  if (!is_safe_subset_call(expression)) {
    stop(
      "Could not safely recover the original model data from `model$call$data`. ",
      "Use a named data frame or a row-filtering base `subset()` expression when fitting the model.",
      call. = FALSE
    )
  }

  arguments <- as.list(expression)[-1L]
  argument_names <- names(arguments)
  if (is.null(argument_names)) argument_names <- rep.int("", length(arguments))
  data_index <- which(argument_names == "x")
  if (!length(data_index)) data_index <- 1L
  subset_index <- which(argument_names == "subset")
  if (!length(subset_index)) {
    candidates <- setdiff(seq_along(arguments), data_index[1L])
    if (length(candidates)) subset_index <- candidates[1L]
  }
  supported <- c(data_index[1L], subset_index[1L])
  supported <- supported[!is.na(supported)]
  if (length(arguments) > length(unique(supported))) {
    stop(
      "Could not safely recover model data from `subset()`: only row filtering is supported.",
      call. = FALSE
    )
  }

  data <- evaluate_model_data_expression(arguments[[data_index[1L]]], environment)
  data <- as_base_data_frame(data)
  if (!length(subset_index)) return(data)

  predicate <- arguments[[subset_index[1L]]]
  if (!is_safe_subset_predicate(predicate)) {
    stop(
      "Could not safely evaluate the row predicate in `model$call$data`.",
      call. = FALSE
    )
  }
  rows <- tryCatch(
    eval(predicate, envir = data, enclos = baseenv()),
    error = function(e) {
      stop(
        "Could not evaluate the row predicate in `model$call$data`: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!is.logical(rows) || !(length(rows) %in% c(1L, nrow(data)))) {
    stop(
      "The row predicate in `model$call$data` must return one logical value per row.",
      call. = FALSE
    )
  }
  rows <- rep_len(rows, nrow(data))
  rows[is.na(rows)] <- FALSE
  data[rows, , drop = FALSE]
}

is_safe_subset_call <- function(expression) {
  if (!is.call(expression)) return(FALSE)
  head <- expression[[1L]]
  if (is.name(head) && identical(as.character(head), "subset")) return(TRUE)
  is.call(head) && identical(as.character(head[[1L]]), "::") &&
    identical(as.character(head[[2L]]), "base") &&
    identical(as.character(head[[3L]]), "subset")
}

is_safe_subset_predicate <- function(expression) {
  if (is.atomic(expression) || is.name(expression)) return(TRUE)
  if (!is.call(expression) || !is.name(expression[[1L]])) return(FALSE)
  allowed <- c(
    "(", "!", "&", "&&", "|", "||", "==", "!=", "<", "<=", ">", ">=",
    "+", "-", "*", "/", "^", "%%", "%/%", "%in%", ":", "c", "is.na",
    "is.finite", "complete.cases", "[", "[[", "$"
  )
  if (!as.character(expression[[1L]]) %in% allowed) return(FALSE)
  all(vapply(as.list(expression)[-1L], is_safe_subset_predicate, logical(1)))
}

as_base_data_frame <- function(x) {
  if (is.data.frame(x)) {
    attributes <- attributes(x)
    attributes$class <- "data.frame"
    attributes(x) <- attributes
    return(x)
  }
  if (is.matrix(x) && length(dim(x)) == 2L) {
    columns <- lapply(seq_len(ncol(x)), function(i) x[, i])
    names(columns) <- colnames(x) %||% paste0("V", seq_len(ncol(x)))
    result <- base::list2DF(columns)
    if (!is.null(rownames(x))) row.names(result) <- rownames(x)
    return(result)
  }
  if (is.list(x) && is.null(class(x))) {
    return(base::list2DF(x))
  }
  stop(
    "The recovered model data must be a data frame, matrix, or unclassed list.",
    call. = FALSE
  )
}

align_recovered_model_data <- function(data, model_frame) {
  used_rows <- row.names(model_frame)
  available_rows <- row.names(data)
  if (anyDuplicated(used_rows) || anyDuplicated(available_rows)) {
    stop(
      "Could not align recovered model data because row names are not unique.",
      call. = FALSE
    )
  }
  positions <- match(used_rows, available_rows)
  if (anyNA(positions)) {
    stop(
      "Could not align the recovered model data with the observations used by the fitted model.",
      call. = FALSE
    )
  }
  aligned <- data[positions, , drop = FALSE]
  if (!identical(row.names(aligned), used_rows)) {
    stop(
      "Recovered model data did not preserve the fitted model's observation order.",
      call. = FALSE
    )
  }
  aligned
}

validate_recovered_model_data <- function(model, data, model_frame) {
  reconstructed <- tryCatch(
    stats::model.frame(
      stats::terms(model), data = data, na.action = stats::na.pass,
      drop.unused.levels = TRUE, xlev = model$xlevels
    ),
    error = function(e) {
      stop(
        "Recovered model data cannot reevaluate the fitted formula: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!identical(row.names(reconstructed), row.names(model_frame))) {
    stop(
      "Recovered model data do not correspond to the observations used by the fitted model.",
      call. = FALSE
    )
  }
  missing <- setdiff(names(reconstructed), names(model_frame))
  if (length(missing)) {
    stop(
      "Recovered model data produced unexpected formula columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  matches <- vapply(names(reconstructed), function(name) {
    formula_column_matches(reconstructed[[name]], model_frame[[name]])
  }, logical(1))
  if (!all(matches)) {
    stop(
      "Recovered model data no longer reproduce the observations used by the fitted model.",
      call. = FALSE
    )
  }
  invisible(data)
}

formula_column_matches <- function(reconstructed, fitted) {
  if (is.matrix(reconstructed) && is.matrix(fitted)) {
    return(
      identical(dim(reconstructed), dim(fitted)) &&
        isTRUE(all.equal(as.vector(reconstructed), as.vector(fitted)))
    )
  }
  isTRUE(all.equal(reconstructed, fitted))
}

has_model_call_argument <- function(model, name) {
  value <- model$call[[name]]
  !is.null(value) && !identical(value, quote(NULL))
}

update_model_for_resample <- function(model, data, weights, offset) {
  if (!is.null(weights) && !is.null(offset)) {
    return(stats::update(
      model, data = data, subset = NULL, weights = weights, offset = offset
    ))
  }
  if (!is.null(weights)) {
    return(stats::update(model, data = data, subset = NULL, weights = weights))
  }
  if (!is.null(offset)) {
    return(stats::update(model, data = data, subset = NULL, offset = offset))
  }
  stats::update(model, data = data, subset = NULL)
}
