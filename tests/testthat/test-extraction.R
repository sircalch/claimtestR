test_that("numeric and matrix inputs are supported", {
  estimates <- c(control = -1, treatment = 2)
  expect_true(expect_positive_effect(estimates, term = "treatment")$passed)

  results <- cbind(
    estimate = c(-1, 2), conf.low = c(-2, 1), conf.high = c(0, 3)
  )
  rownames(results) <- c("control", "treatment")
  expect_true(expect_interval_excludes(results, term = "treatment")$passed)
})

test_that("data frames and broom-style results are supported", {
  results <- data.frame(
    term = c("control", "treatment"),
    estimate = c(0, 1.2),
    conf.low = c(-0.1, 0.4),
    conf.high = c(0.1, 2)
  )
  expect_true(expect_positive_effect(results, "treatment")$passed)
  expect_identical(expect_positive_effect(results, "treatment")$term, "treatment")

  custom <- data.frame(parameter = "x", beta = 2, lower = 1, upper = 3)
  expect_true(expect_positive_effect(custom, "x", estimate = "beta")$passed)
  expect_true(expect_interval_excludes(custom, "x", estimate = "beta")$passed)
})

test_that("lm and glm inputs expose estimates and intervals", {
  linear <- lm(mpg ~ wt, data = mtcars)
  logistic <- glm(am ~ wt, data = mtcars, family = binomial())

  expect_true(expect_negative_effect(linear, term = "wt")$passed)
  expect_true(expect_interval_excludes(linear, term = "wt")$passed)
  expect_true(expect_negative_effect(logistic, term = "wt")$passed)
  expect_true(expect_interval_excludes(logistic, term = "wt")$passed)
})

test_that("all public model claims reject non-converged glm objects", {
  converged <- glm(am ~ wt, data = mtcars, family = binomial())
  expect_true(converged$converged)

  claim_calls <- list(
    positive = function(x) expect_positive_effect(x, term = "wt"),
    negative = function(x) expect_negative_effect(x, term = "wt"),
    interval_excludes = function(x) {
      expect_interval_excludes(x, term = "wt", value = 0)
    },
    interval_includes = function(x) {
      expect_interval_includes(x, term = "wt", value = 0)
    },
    practical = function(x) {
      expect_practical_effect(x, minimum = 0.1, term = "wt")
    },
    equivalent = function(x) {
      claimtestR::expect_equivalent(x, bounds = c(-100, 100), term = "wt")
    },
    stable = function(x) {
      expect_stable_direction(
        x, term = "wt", method = "leave_one_out",
        minimum_proportion = 0.5
      )
    }
  )

  for (claim_call in claim_calls) {
    expect_s3_class(claim_call(converged), "claim_test")
  }

  manipulated <- converged
  manipulated$converged <- FALSE
  for (claim_call in claim_calls) {
    expect_error(
      claim_call(manipulated),
      "cannot be evaluated reliably.*did not converge"
    )
  }
})

test_that("a genuinely non-converged glm is rejected with actionable guidance", {
  non_converged <- suppressWarnings(glm(
    am ~ wt, data = mtcars, family = binomial(),
    control = glm.control(maxit = 1)
  ))

  expect_false(non_converged$converged)
  expect_error(
    expect_negative_effect(non_converged, term = "wt"),
    "Refit the model and verify convergence"
  )
})

test_that("extraction rejects ambiguous and malformed inputs", {
  multiple <- data.frame(term = c("a", "b"), estimate = c(1, 2))
  expect_error(expect_positive_effect(multiple), "term.*required")
  expect_error(expect_positive_effect(multiple, "missing"), "exactly one row")
  expect_error(expect_positive_effect(data.frame(term = "a", value = 1)), "estimate column")
  expect_error(expect_positive_effect(data.frame(estimate = NA_real_)), "finite numeric")
  expect_error(
    expect_interval_excludes(data.frame(estimate = 1, low = 0, high = 2),
                             conf.int = c("bad", "high")),
    "Unknown lower"
  )
  expect_error(
    expect_interval_excludes(data.frame(estimate = 1, conf.low = 2, conf.high = 1)),
    "lower confidence limit"
  )
  expect_error(expect_positive_effect(list(estimate = 1)), "Unsupported")
  expect_error(expect_positive_effect(1, level = 1), "level")
})
