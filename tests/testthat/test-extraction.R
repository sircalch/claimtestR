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

