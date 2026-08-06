test_that("active data bindings are rejected before they execute", {
  fit <- local({
    model_data <- mtcars
    lm(mpg ~ log(wt), data = model_data)
  })
  formula_environment <- environment(stats::formula(fit))
  rm("model_data", envir = formula_environment)

  effects <- new.env(parent = emptyenv())
  effects$executed <- FALSE
  payload_file <- tempfile("claimtestR-active-binding-")
  payload_option <- "claimtestR.active.binding.probe"
  payload_global <- ".claimtestR_active_binding_probe"
  old_option <- getOption(payload_option)
  old_wd <- getwd()
  set.seed(812)
  old_seed <- .Random.seed
  old_globals <- ls(.GlobalEnv, all.names = TRUE)
  on.exit({
    if (exists("model_data", envir = formula_environment, inherits = FALSE)) {
      rm("model_data", envir = formula_environment)
    }
    if (file.exists(payload_file)) unlink(payload_file)
    setwd(old_wd)
    options(structure(list(old_option), names = payload_option))
    if (exists(payload_global, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = payload_global, envir = .GlobalEnv)
    }
  }, add = TRUE)

  makeActiveBinding("model_data", function(value) {
    effects$executed <- TRUE
    file.create(payload_file)
    options(structure(list(TRUE), names = payload_option))
    setwd(tempdir())
    assign(payload_global, TRUE, envir = .GlobalEnv)
    mtcars
  }, formula_environment)

  expect_error(
    expect_stable_direction(
      fit, term = "log(wt)", method = "bootstrap", iterations = 2
    ),
    "active binding"
  )
  expect_false(effects$executed)
  expect_false(file.exists(payload_file))
  expect_identical(getOption(payload_option), old_option)
  expect_identical(getwd(), old_wd)
  expect_identical(.Random.seed, old_seed)
  expect_identical(ls(.GlobalEnv, all.names = TRUE), old_globals)
})

test_that("only simple named data frames are accepted", {
  materialized <- subset(mtcars, mpg > 15)
  allowed <- lm(mpg ~ log(wt), data = materialized)
  allowed_result <- expect_stable_direction(
    allowed, term = "log(wt)", method = "bootstrap",
    iterations = 5, seed = 1, minimum_proportion = 0
  )
  expect_identical(allowed_result$details$failed, 0L)

  unsupported <- list(
    subset = lm(mpg ~ wt, data = subset(mtcars, mpg > 15)),
    base_subset = lm(mpg ~ wt, data = base::subset(mtcars, mpg > 15)),
    transform = lm(mpg ~ z, data = transform(mtcars, z = wt^2)),
    within = lm(mpg ~ z, data = within(mtcars, z <- wt^2)),
    get = lm(mpg ~ wt, data = get("mtcars")),
    as_is = lm(mpg ~ wt, data = I(mtcars))
  )
  terms <- c(
    subset = "wt", base_subset = "wt", transform = "z",
    within = "z", get = "wt", as_is = "wt"
  )

  for (name in names(unsupported)) {
    expect_error(
      expect_stable_direction(
        unsupported[[name]], term = terms[[name]],
        method = "bootstrap", iterations = 2
      ),
      "simple data-frame name",
      info = name
    )
  }
})

test_that("manipulated data calls are rejected without side effects", {
  model_data <- mtcars
  fit <- lm(mpg ~ log(wt), data = model_data)
  effects <- new.env(parent = emptyenv())
  effects$executed <- FALSE
  payload <- function() {
    effects$executed <- TRUE
    model_data
  }
  payload_file <- tempfile("claimtestR-data-call-")
  payload_global <- ".claimtestR_data_call_probe"
  payload_option <- "claimtestR.data.call.probe"
  old_option <- getOption(payload_option)
  old_wd <- getwd()
  old_globals <- ls(.GlobalEnv, all.names = TRUE)
  on.exit({
    if (file.exists(payload_file)) unlink(payload_file)
    setwd(old_wd)
    options(structure(list(old_option), names = payload_option))
    if (exists(payload_global, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = payload_global, envir = .GlobalEnv)
    }
  }, add = TRUE)

  calls <- list(
    quote(system(payload())),
    quote(source(payload())),
    quote(file.create(payload_file)),
    quote(assign(payload_global, payload(), envir = .GlobalEnv)),
    quote(options(claimtestR.data.call.probe = payload())),
    quote(setwd(payload())),
    quote(eval(payload())),
    quote(do.call(payload, list())),
    quote(get(payload())),
    quote(subset(model_data, payload())),
    quote(base::subset(model_data, payload())),
    quote(transform(model_data, z = payload())),
    quote(within(model_data, z <- payload())),
    quote(subset(transform(model_data, z = wt^2), payload()))
  )

  for (data_call in calls) {
    manipulated <- fit
    manipulated$call$data <- data_call
    expect_error(
      expect_stable_direction(
        manipulated, term = "log(wt)", method = "bootstrap",
        iterations = 2
      ),
      "simple data-frame name",
      info = deparse(data_call)
    )
  }

  expect_false(effects$executed)
  expect_false(file.exists(payload_file))
  expect_identical(getOption(payload_option), old_option)
  expect_identical(getwd(), old_wd)
  expect_identical(ls(.GlobalEnv, all.names = TRUE), old_globals)
})

test_that("custom S3 classes and methods are never dispatched for recovery", {
  methods_called <- new.env(parent = emptyenv())
  methods_called$bracket <- FALSE
  methods_called$coerce <- FALSE
  methods_called$subset <- FALSE
  `[.claimtest_probe` <- function(x, ...) {
    methods_called$bracket <- TRUE
    NextMethod("[")
  }
  as.data.frame.claimtest_probe <- function(x, ...) {
    methods_called$coerce <- TRUE
    NextMethod("as.data.frame")
  }
  subset.claimtest_probe <- function(x, ...) {
    methods_called$subset <- TRUE
    NextMethod("subset")
  }

  model_data <- mtcars
  model_data$probe <- structure(
    rep(TRUE, nrow(model_data)), class = c("claimtest_probe", "logical")
  )
  fit <- lm(mpg ~ log(wt), data = model_data)
  expect_error(
    expect_stable_direction(
      fit, term = "log(wt)", method = "bootstrap", iterations = 2
    ),
    "unclassed atomic columns or base factors"
  )
  manipulated <- fit
  manipulated$call$data <- quote(subset(model_data, probe[1]))
  expect_error(
    expect_stable_direction(
      manipulated, term = "log(wt)", method = "bootstrap", iterations = 2
    ),
    "simple data-frame name"
  )

  custom_data <- model_data
  class(custom_data) <- c("claimtest_probe", "data.frame")
  manipulated <- fit
  manipulated$call$data <- quote(custom_data)
  expect_error(
    expect_stable_direction(
      manipulated, term = "log(wt)", method = "bootstrap", iterations = 2
    ),
    "class exactly `data.frame`"
  )

  manipulated$call$data <- quote(subset(custom_data, mpg > 15))
  expect_error(
    expect_stable_direction(
      manipulated, term = "log(wt)", method = "bootstrap", iterations = 2
    ),
    "simple data-frame name"
  )
  expect_false(methods_called$bracket)
  expect_false(methods_called$coerce)
  expect_false(methods_called$subset)
})

test_that("custom S3 columns are rejected before any generic dispatch", {
  method_names <- c(
    "dim.probe", "length.probe", "names.probe", "is.na.probe",
    "[.probe", "[[.probe", "as.data.frame.probe", "Ops.probe"
  )
  method_calls <- stats::setNames(integer(length(method_names)), method_names)
  old_methods <- lapply(method_names, function(name) {
    get0(name, envir = .GlobalEnv, inherits = FALSE)
  })
  method_existed <- vapply(
    method_names, exists, logical(1), envir = .GlobalEnv, inherits = FALSE
  )
  on.exit({
    for (index in seq_along(method_names)) {
      name <- method_names[[index]]
      if (method_existed[[index]]) {
        assign(name, old_methods[[index]], envir = .GlobalEnv)
      } else if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
        rm(list = name, envir = .GlobalEnv)
      }
    }
  }, add = TRUE)

  model_data <- mtcars
  model_data$probe <- structure(
    rep(TRUE, nrow(model_data)), class = c("probe", "logical")
  )
  fit <- lm(mpg ~ wt, data = model_data)
  for (name in method_names) {
    assign(name, local({
      method_name <- name
      function(...) {
        method_calls[[method_name]] <<- method_calls[[method_name]] + 1L
        NULL
      }
    }), envir = .GlobalEnv)
  }
  method_calls[] <- 0L

  expect_error(
    expect_stable_direction(
      fit, term = "wt", method = "bootstrap", iterations = 2
    ),
    "unclassed atomic columns or base factors"
  )
  expect_identical(unname(method_calls), rep(0L, length(method_names)))
  expect_false(method_calls[["dim.probe"]] > 0L, info = "DIM_METHOD_CALLED")
})

test_that("only base atomic vectors and base factors pass column validation", {
  safe_columns <- list(
    numeric = c(1, 2),
    integer = c(1L, 2L),
    logical = c(TRUE, FALSE),
    character = c("a", "b"),
    factor = factor(c("a", "b")),
    ordered = ordered(c("a", "b"))
  )
  expect_true(all(vapply(
    safe_columns, claimtestR:::is_safe_data_column, logical(1)
  )))

  custom_factor <- factor(c("a", "b"))
  class(custom_factor) <- c("probe", "factor")
  dimensioned_factor <- factor(c("a", "b"))
  attr(dimensioned_factor, "dim") <- c(2L, 1L)
  custom_levels <- factor(c("a", "b"))
  attr(custom_levels, "levels") <- structure(
    attr(custom_levels, "levels", exact = TRUE), class = "probe"
  )

  expect_false(claimtestR:::is_safe_data_column(custom_factor))
  expect_false(claimtestR:::is_safe_data_column(dimensioned_factor))
  expect_false(claimtestR:::is_safe_data_column(custom_levels))
})

test_that("S4 columns are rejected before S4 method dispatch", {
  class_name <- "claimtestR_s4_dispatch_probe"
  class_environment <- environment()
  method_calls <- new.env(parent = emptyenv())
  method_calls$dim <- 0L
  method_calls$length <- 0L
  method_calls$subset <- 0L

  methods::setClass(class_name, contains = "numeric", where = class_environment)
  methods::setMethod(
    "dim", signature(x = class_name),
    function(x) {
      method_calls$dim <- method_calls$dim + 1L
      NULL
    },
    where = class_environment
  )
  methods::setMethod(
    "length", signature(x = class_name),
    function(x) {
      method_calls$length <- method_calls$length + 1L
      0L
    },
    where = class_environment
  )
  methods::setMethod(
    "[", signature(x = class_name),
    function(x, i, j, ..., drop = TRUE) {
      method_calls$subset <- method_calls$subset + 1L
      x
    },
    where = class_environment
  )
  on.exit({
    methods::removeMethod("dim", signature(x = class_name), where = class_environment)
    methods::removeMethod("length", signature(x = class_name), where = class_environment)
    methods::removeMethod("[", signature(x = class_name), where = class_environment)
    methods::removeClass(class_name, where = class_environment)
  }, add = TRUE)

  model_data <- mtcars
  fit <- lm(mpg ~ wt, data = model_data)
  probe <- methods::new(class_name, rep(1, nrow(model_data)))
  columns <- lapply(seq_along(model_data), function(index) {
    base::.subset2(model_data, index)
  })
  attr(columns, "names") <- attr(model_data, "names", exact = TRUE)
  columns$probe <- probe
  model_data <- structure(
    columns,
    row.names = attr(model_data, "row.names", exact = TRUE),
    class = "data.frame"
  )
  global_names <- ls(.GlobalEnv, all.names = TRUE)

  expect_error(
    expect_stable_direction(
      fit, term = "wt", method = "bootstrap", iterations = 2
    ),
    "unclassed atomic columns or base factors"
  )
  expect_identical(method_calls$dim, 0L)
  expect_identical(method_calls$length, 0L)
  expect_identical(method_calls$subset, 0L)
  expect_identical(ls(.GlobalEnv, all.names = TRUE), global_names)
})

test_that("S4 and inaccessible data objects fail explicitly", {
  class_name <- "claimtestR_security_probe_frame"
  class_environment <- environment()
  methods::setClass(
    class_name, slots = c(data = "data.frame"), where = class_environment
  )
  on.exit(methods::removeClass(class_name, where = class_environment), add = TRUE)

  model_data <- mtcars
  fit <- lm(mpg ~ log(wt), data = model_data)
  s4_data <- methods::new(class_name, data = mtcars)
  manipulated <- fit
  manipulated$call$data <- quote(s4_data)
  expect_error(
    expect_stable_direction(
      manipulated, term = "log(wt)", method = "bootstrap", iterations = 2
    ),
    "class exactly `data.frame`"
  )

  manipulated$call$data <- quote(does_not_exist)
  expect_error(
    expect_stable_direction(
      manipulated, term = "log(wt)", method = "bootstrap", iterations = 2
    ),
    "Could not recover.*does_not_exist"
  )

  attr(manipulated$terms, ".Environment") <- emptyenv()
  expect_error(
    expect_stable_direction(
      manipulated, term = "log(wt)", method = "bootstrap", iterations = 2
    ),
    "formula environment or its parents"
  )
})

test_that("data symbols are located in the exact parent environment", {
  binding_parent <- new.env(parent = globalenv())
  binding_parent$model_data <- mtcars
  formula_environment <- new.env(parent = binding_parent)
  stored_formula <- stats::as.formula(
    "mpg ~ log(wt)", env = formula_environment
  )
  fit <- lm(stored_formula, data = mtcars)
  fit$call$data <- quote(model_data)

  result <- expect_stable_direction(
    fit, term = "log(wt)", method = "bootstrap",
    iterations = 5, seed = 1, minimum_proportion = 0
  )
  expect_identical(result$details$failed, 0L)

  binding_executed <- FALSE
  rm("model_data", envir = binding_parent)
  makeActiveBinding("model_data", function(value) {
    binding_executed <<- TRUE
    mtcars
  }, binding_parent)
  on.exit(rm("model_data", envir = binding_parent), add = TRUE)
  expect_error(
    expect_stable_direction(
      fit, term = "log(wt)", method = "bootstrap", iterations = 2
    ),
    "active binding"
  )
  expect_false(binding_executed)
})
