prepare_model_resampling <- function(model) {
  model_frame <- model$model
  if (!identical(class(model_frame), "data.frame")) {
    stop(
      "Model resampling requires an lm or glm fitted with `model = TRUE` ",
      "so the observations actually used can be recovered safely.",
      call. = FALSE
    )
  }
  recovered <- recover_original_model_data(model)
  data <- align_recovered_model_data(recovered, model_frame)
  validate_recovered_model_data(model, data, model_frame)

  weights <- NULL
  if (has_model_call_argument(model, "weights")) {
    weight_index <- match("(weights)", data_frame_names(model_frame))
    weights <- if (is.na(weight_index)) {
      NULL
    } else {
      base::.subset2(model_frame, weight_index)
    }
    if (is.null(weights) || length(weights) != data_frame_nrow(model_frame)) {
      stop(
        "Could not recover the weights used by the fitted model for refitting.",
        call. = FALSE
      )
    }
  }

  offset <- NULL
  if (has_model_call_argument(model, "offset")) {
    offset_index <- match("(offset)", data_frame_names(model_frame))
    if (is.na(offset_index)) {
      stop(
        "Could not recover the call-level offset used by the fitted model for refitting.",
        call. = FALSE
      )
    }
    offset <- base::.subset2(model_frame, offset_index)
  }

  list(data = data, weights = weights, offset = offset)
}

recover_original_model_data <- function(model) {
  data_expression <- model$call$data
  if (!is.name(data_expression)) {
    stop(
      "Model stability refits require `data` to be supplied as a simple ",
      "data-frame name. Calls such as `subset()`, `transform()`, `within()`, ",
      "or `get()` are not supported; assign their result to a named base ",
      "data frame before fitting the model.",
      call. = FALSE
    )
  }

  formula_environment <- environment(stats::formula(model))
  if (is.null(formula_environment)) {
    stop(
      "Could not safely recover the original model data because the model formula has no environment.",
      call. = FALSE
    )
  }

  name <- as.character(data_expression)
  binding_environment <- find_binding_environment(name, formula_environment)
  if (is.null(binding_environment)) {
    stop(
      "Could not recover the original model data object `", name,
      "` from the model formula environment or its parents.",
      call. = FALSE
    )
  }
  if (bindingIsActive(name, binding_environment)) {
    stop(
      "Cannot safely recover model data from `", name,
      "` because it is an active binding. Use an ordinary named base data frame.",
      call. = FALSE
    )
  }

  data <- get(name, envir = binding_environment, inherits = FALSE)
  if (!identical(class(data), "data.frame")) {
    stop(
      "The model data object `", name,
      "` must have class exactly `data.frame` for safe stability refits. ",
      "Objects with additional or custom classes are not supported in version 0.1.2.",
      call. = FALSE
    )
  }
  data_names <- data_frame_names(data)
  safe_columns <- vapply(seq_along(data_names), function(index) {
    is_safe_data_column(base::.subset2(data, index))
  }, logical(1), USE.NAMES = FALSE)
  unsafe_columns <- data_names[!safe_columns]
  if (length(unsafe_columns)) {
    stop(
      "The model data object `", name,
      "` contains columns with additional classes or non-atomic structure: ",
      paste(unsafe_columns, collapse = ", "),
      ". Stability refits in version 0.1.2 require unclassed atomic columns ",
      "or base factors.",
      call. = FALSE
    )
  }

  required <- all.vars(stats::formula(model))
  missing <- setdiff(required, data_names)
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

data_frame_names <- function(data) {
  attr(data, "names", exact = TRUE)
}

is_safe_data_column <- function(column) {
  if (isS4(column)) return(FALSE)
  if (is.object(column)) return(is_safe_base_factor(column))
  if (!typeof(column) %in% c(
    "logical", "integer", "double", "complex", "character", "raw"
  )) {
    return(FALSE)
  }
  is.null(attr(column, "dim", exact = TRUE))
}

is_safe_base_factor <- function(column) {
  column_class <- attr(column, "class", exact = TRUE)
  if (!identical(column_class, "factor") &&
      !identical(column_class, c("ordered", "factor"))) {
    return(FALSE)
  }
  if (!identical(typeof(column), "integer") ||
      !is.null(attr(column, "dim", exact = TRUE))) {
    return(FALSE)
  }
  levels <- attr(column, "levels", exact = TRUE)
  identical(typeof(levels), "character") &&
    !is.object(levels) && !isS4(levels) &&
    is.null(attr(levels, "dim", exact = TRUE))
}

find_binding_environment <- function(name, start_environment) {
  current <- start_environment
  repeat {
    if (exists(name, envir = current, inherits = FALSE)) return(current)
    if (identical(current, emptyenv())) return(NULL)
    current <- parent.env(current)
  }
}

align_recovered_model_data <- function(data, model_frame) {
  used_rows <- data_frame_row_names(model_frame)
  available_rows <- data_frame_row_names(data)
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
  data_names <- data_frame_names(data)
  columns <- lapply(seq_along(data_names), function(index) {
    subset_safe_data_column(base::.subset2(data, index), positions)
  })
  attr(columns, "names") <- data_names
  aligned <- structure(columns, row.names = used_rows, class = "data.frame")
  if (!identical(data_frame_row_names(aligned), used_rows)) {
    stop(
      "Recovered model data did not preserve the fitted model's observation order.",
      call. = FALSE
    )
  }
  aligned
}

data_frame_row_names <- function(data) {
  rows <- base::.row_names_info(data, type = 0L)
  if (is.integer(rows) && length(rows) == 2L && is.na(rows[1L])) {
    return(as.character(seq_len(abs(rows[2L]))))
  }
  as.character(rows)
}

data_frame_nrow <- function(data) {
  base::.row_names_info(data, type = 2L)
}

subset_safe_data_column <- function(column, positions) {
  if (!is.object(column)) return(base::.subset(column, positions))
  result <- base::.subset(unclass(column), positions)
  attr(result, "levels") <- attr(column, "levels", exact = TRUE)
  class(result) <- class(column)
  result
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
  if (!identical(
    data_frame_row_names(reconstructed), data_frame_row_names(model_frame)
  )) {
    stop(
      "Recovered model data do not correspond to the observations used by the fitted model.",
      call. = FALSE
    )
  }
  reconstructed_names <- data_frame_names(reconstructed)
  model_frame_names <- data_frame_names(model_frame)
  missing <- setdiff(reconstructed_names, model_frame_names)
  if (length(missing)) {
    stop(
      "Recovered model data produced unexpected formula columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  matches <- vapply(reconstructed_names, function(name) {
    reconstructed_column <- base::.subset2(
      reconstructed, match(name, reconstructed_names)
    )
    fitted_column <- base::.subset2(model_frame, match(name, model_frame_names))
    formula_column_matches(reconstructed_column, fitted_column)
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
  arguments <- list(
    object = model,
    formula. = stats::formula(model),
    data = data,
    subset = NULL,
    na.action = stats::na.pass
  )
  if (inherits(model, "glm")) arguments$family <- stats::family(model)
  if (!is.null(weights)) arguments$weights <- weights
  if (!is.null(offset)) arguments$offset <- offset
  do.call(stats::update, arguments)
}
