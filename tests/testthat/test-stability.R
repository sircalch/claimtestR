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

test_that("model improvement respects metric direction", {
  expect_true(expect_model_improves(8, 10, "lower")$passed)
  expect_true(expect_model_improves(0.9, 0.8, "higher", minimum = 0.05)$passed)
  expect_false(expect_model_improves(10, 10, "lower")$passed)
})
