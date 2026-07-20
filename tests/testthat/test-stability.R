test_that("numeric stability uses a unique modal non-zero direction", {
  stable <- expect_stable_direction(c(rep(1, 19), -1), 0.95)
  unstable <- expect_stable_direction(c(rep(1, 9), -1), 0.95)
  tied <- expect_stable_direction(c(-1, 1), minimum_proportion = 0.5)

  expect_true(stable$passed)
  expect_identical(stable$details$direction, "positive")
  expect_false(unstable$passed)
  expect_false(tied$passed)
})

test_that("stability handles invalid and missing estimates", {
  expect_error(expect_stable_direction(1), "At least two")
  expect_error(expect_stable_direction(c(1, NA)), "missing")
  expect_true(expect_stable_direction(c(1, 1, NA), na.rm = TRUE)$passed)
  expect_error(expect_stable_direction(c(1, 2), minimum_proportion = 1.1), "<= 1")
  expect_error(expect_stable_direction(c(1, 2), method = "bootstrap"), "Numeric")
})

test_that("lm bootstrap and leave-one-out stability are reproducible", {
  fit <- lm(mpg ~ wt, data = mtcars)
  set.seed(7)
  state <- .Random.seed
  bootstrap <- expect_stable_direction(
    fit, term = "wt", method = "bootstrap", iterations = 20,
    minimum_proportion = 0.8, seed = 42
  )
  leave_one_out <- expect_stable_direction(
    fit, term = "wt", method = "leave_one_out", minimum_proportion = 0.8
  )

  expect_identical(.Random.seed, state)
  expect_true(bootstrap$passed)
  expect_identical(bootstrap$details$method, "bootstrap")
  expect_length(bootstrap$details$estimates, 20)
  expect_identical(bootstrap$details$attempted, 20L)
  expect_identical(bootstrap$details$successful, 20L)
  expect_identical(bootstrap$details$failed, 0L)
  expect_equal(bootstrap$details$success_proportion, 1)
  expect_true(bootstrap$details$resampling_valid)
  expect_equal(nrow(bootstrap$details$failures), 0)
  expect_true(leave_one_out$passed)
  expect_length(leave_one_out$details$estimates, nrow(mtcars))

  repeated <- expect_stable_direction(
    fit, term = "wt", method = "bootstrap", iterations = 20,
    minimum_proportion = 0.8, seed = 42
  )
  expect_identical(repeated$details$estimates, bootstrap$details$estimates)
  expect_identical(.Random.seed, state)
})

test_that("bootstrap reports partial refit failures without dropping them", {
  rare_data <- data.frame(
    response = seq_len(10),
    group = factor(c(rep("common", 9), "rare"))
  )
  fit <- lm(response ~ group, data = rare_data)

  result <- expect_stable_direction(
    fit, term = "grouprare", method = "bootstrap", iterations = 100,
    minimum_proportion = 0.5, seed = 42
  )

  expect_identical(result$details$attempted, 100L)
  expect_gt(result$details$failed, 0L)
  expect_lt(result$details$failed, result$details$attempted)
  expect_identical(
    result$details$successful + result$details$failed,
    result$details$attempted
  )
  expect_equal(
    result$details$success_proportion,
    result$details$successful / result$details$attempted
  )
  expect_length(result$details$estimates, result$details$attempted)
  expect_equal(sum(is.na(result$details$estimates)), result$details$failed)
  expect_equal(nrow(result$details$failures), result$details$failed)
  expect_match(result$evidence, "refits succeeded")
  expect_false(result$passed)
})

test_that("leave-one-out returns a failed claim when every refit fails", {
  method_name <- "update.claimtest_always_fail"
  old_method <- get0(method_name, envir = .GlobalEnv, inherits = FALSE)
  assign(
    method_name,
    function(object, ...) stop("forced refit failure"),
    envir = .GlobalEnv
  )
  on.exit({
    if (is.null(old_method)) {
      rm(list = method_name, envir = .GlobalEnv)
    } else {
      assign(method_name, old_method, envir = .GlobalEnv)
    }
  }, add = TRUE)

  fit <- lm(mpg ~ wt, data = mtcars)
  class(fit) <- c("claimtest_always_fail", class(fit))

  result <- expect_stable_direction(
    fit, term = "wt", method = "leave_one_out",
    minimum_proportion = 0.5
  )

  expect_false(result$passed)
  expect_false(result$details$resampling_valid)
  expect_identical(result$details$attempted, nrow(mtcars))
  expect_identical(result$details$successful, 0L)
  expect_identical(result$details$failed, nrow(mtcars))
  expect_equal(result$details$success_proportion, 0)
  expect_true(all(is.na(result$details$estimates)))
  expect_true(all(result$details$failures$class == "simpleError"))
  expect_true(all(grepl("forced refit failure", result$details$failures$message)))
  expect_match(result$message, "insufficient valid refits")
})

test_that("model resampling rejects samples that are too small", {
  fit <- lm(response ~ 1, data = data.frame(response = c(1, 2)))
  expect_error(
    expect_stable_direction(
      fit, term = "(Intercept)", method = "leave_one_out"
    ),
    "at least three observations"
  )
})

test_that("a term absent after refitting has an explicit diagnostic", {
  refit <- structure(
    list(coefficients = c(other = 1), converged = TRUE),
    class = c("glm", "lm")
  )
  outcome <- claimtestR:::assess_refit(refit, "target")

  expect_false(outcome$success)
  expect_identical(outcome$failure_class, "term_not_found")
  expect_match(outcome$failure_message, "absent after refitting")
})

test_that("non-converged glm bootstrap refits are recorded as failures", {
  set.seed(1)
  sample_data <- data.frame(
    x = rnorm(30)
  )
  sample_data$y <- rbinom(30, 1, plogis(2 * sample_data$x - 0.5))
  fit <- glm(
    y ~ x, data = sample_data, family = binomial(),
    control = glm.control(maxit = 4)
  )
  expect_true(fit$converged)

  result <- expect_stable_direction(
    fit, term = "x", method = "bootstrap", iterations = 40,
    minimum_proportion = 0.5, seed = 1
  )

  expect_gt(result$details$failed, 0L)
  expect_lt(result$details$failed, result$details$attempted)
  expect_true("glm_non_convergence" %in% result$details$failures$class)
  expect_gt(nrow(result$details$warnings), 0L)
  expect_false(result$details$resampling_valid)
  expect_false(result$passed)
})

test_that("the caller's random state and options survive success and failure", {
  fit <- lm(mpg ~ wt, data = mtcars)
  set.seed(731)
  state <- .Random.seed
  original_options <- options()

  expect_s3_class(
    expect_stable_direction(
      fit, term = "wt", method = "bootstrap", iterations = 10, seed = 99
    ),
    "claim_test"
  )
  expect_identical(.Random.seed, state)
  expect_identical(options(), original_options)

  expect_error(
    claimtestR:::with_preserved_seed(99, stop("forced failure")),
    "forced failure"
  )
  expect_identical(.Random.seed, state)
  expect_identical(options(), original_options)
})

test_that("transformed lm and glm formulas refit from original data", {
  models <- list(
    predictor = lm(mpg ~ log(wt), data = mtcars),
    response = lm(log(mpg) ~ wt, data = mtcars),
    as_is = lm(mpg ~ I(wt^2), data = mtcars),
    interaction = lm(mpg ~ log(wt) * factor(am), data = mtcars),
    polynomial = lm(mpg ~ poly(wt, 2), data = mtcars),
    glm = glm(am ~ log(wt), data = mtcars, family = binomial())
  )
  terms <- c(
    predictor = "log(wt)", response = "wt", as_is = "I(wt^2)",
    interaction = "log(wt)", polynomial = "poly(wt, 2)1", glm = "log(wt)"
  )

  for (name in names(models)) {
    bootstrap <- expect_stable_direction(
      models[[name]], term = terms[[name]], method = "bootstrap",
      iterations = 10, seed = 123, minimum_proportion = 0
    )
    leave_one_out <- expect_stable_direction(
      models[[name]], term = terms[[name]], method = "leave_one_out",
      minimum_proportion = 0
    )

    expect_true(bootstrap$details$resampling_valid, info = name)
    expect_true(bootstrap$details$successful >= 8L, info = name)
    expect_true(leave_one_out$details$resampling_valid, info = name)
    expect_identical(leave_one_out$details$failed, 0L, info = name)
    expect_false(
      any(grepl("object .* not found", bootstrap$details$failures$message)),
      info = name
    )
  }
})

test_that("recovered data match rows retained after missing values", {
  data <- mtcars
  data$wt[c(2, 7)] <- NA_real_
  data$mpg[5] <- NA_real_
  fit <- lm(log(mpg) ~ log(wt), data = data, na.action = na.exclude)
  prepared <- claimtestR:::prepare_model_resampling(fit)
  used_rows <- row.names(stats::model.frame(fit))

  expect_identical(row.names(prepared$data), used_rows)
  expect_identical(nrow(prepared$data), length(used_rows))
  expect_false(any(row.names(data)[c(2, 5, 7)] %in% row.names(prepared$data)))

  result <- expect_stable_direction(
    fit, term = "log(wt)", method = "leave_one_out",
    minimum_proportion = 0
  )
  expect_identical(result$details$attempted, length(used_rows))
  expect_identical(result$details$failed, 0L)
})

test_that("subset weights and offsets remain aligned for refits", {
  data <- mtcars
  data$mpg[2] <- NA_real_
  original_data <- data
  fit <- lm(
    log(mpg) ~ log(wt), data = data, subset = cyl >= 4,
    weights = hp, offset = qsec / 100, na.action = na.exclude
  )
  model_frame <- stats::model.frame(fit)
  prepared <- claimtestR:::prepare_model_resampling(fit)

  expect_identical(row.names(prepared$data), row.names(model_frame))
  expect_identical(data, original_data)
  expect_equal(prepared$weights, stats::model.weights(model_frame))
  expect_equal(prepared$offset, model_frame[["(offset)"]])

  bootstrap <- expect_stable_direction(
    fit, term = "log(wt)", method = "bootstrap", iterations = 10,
    seed = 2, minimum_proportion = 0
  )
  leave_one_out <- expect_stable_direction(
    fit, term = "log(wt)", method = "leave_one_out",
    minimum_proportion = 0
  )
  expect_identical(bootstrap$details$failed, 0L)
  expect_identical(leave_one_out$details$failed, 0L)
})

test_that("poly terms remain recoverable after subset and missing-value filtering", {
  data <- mtcars
  data$mpg[2] <- NA_real_
  fit <- lm(
    mpg ~ poly(wt, 2), data = data, subset = cyl >= 4,
    na.action = na.exclude
  )

  result <- expect_stable_direction(
    fit, term = "poly(wt, 2)1", method = "leave_one_out",
    minimum_proportion = 0
  )

  expect_identical(result$details$attempted, nrow(stats::model.frame(fit)))
  expect_identical(result$details$failed, 0L)
})

test_that("named subset expressions and local data objects are recoverable", {
  subset_fit <- lm(
    mpg ~ log(wt), data = subset(mtcars, mpg > 15)
  )
  make_local_fit <- function() {
    local_data <- mtcars
    lm(mpg ~ log(wt), data = local_data)
  }
  local_fit <- make_local_fit()

  subset_result <- expect_stable_direction(
    subset_fit, term = "log(wt)", method = "bootstrap",
    iterations = 5, seed = 1, minimum_proportion = 0
  )
  local_result <- expect_stable_direction(
    local_fit, term = "log(wt)", method = "leave_one_out",
    minimum_proportion = 0
  )

  expect_identical(subset_result$details$failed, 0L)
  expect_identical(local_result$details$failed, 0L)
})

test_that("unsafe data-producing calls are not reevaluated", {
  called <- FALSE
  producer <- function() {
    called <<- TRUE
    mtcars
  }
  fit <- lm(mpg ~ log(wt), data = producer())
  called <- FALSE

  expect_error(
    expect_stable_direction(
      fit, term = "log(wt)", method = "bootstrap", iterations = 2
    ),
    "Could not safely recover the original model data"
  )
  expect_false(called)
})

test_that("models without retained model frames fail without reevaluation", {
  called <- FALSE
  producer <- function() {
    called <<- TRUE
    mtcars
  }
  fit <- lm(mpg ~ wt, data = producer(), model = FALSE)
  called <- FALSE

  expect_error(
    expect_stable_direction(
      fit, term = "wt", method = "bootstrap", iterations = 2
    ),
    "fitted with `model = TRUE`"
  )
  expect_false(called)
})

test_that("missing original variables produce a clear recovery error", {
  fit <- local({
    response <- mtcars$mpg
    predictor <- mtcars$wt
    lm(response ~ log(predictor))
  })

  expect_error(
    expect_stable_direction(
      fit, term = "log(predictor)", method = "leave_one_out"
    ),
    "original model data columns required for refitting: predictor"
  )
})

test_that("model improvement respects metric direction", {
  expect_true(expect_model_improves(8, 10, "lower")$passed)
  expect_true(expect_model_improves(0.9, 0.8, "higher", minimum = 0.05)$passed)
  expect_false(expect_model_improves(10, 10, "lower")$passed)
})
