test_that("direction claims work with numeric values", {
  expect_true(expect_positive_effect(2)$passed)
  expect_false(expect_positive_effect(-2)$passed)
  expect_true(expect_negative_effect(-2)$passed)
})

test_that("data-frame terms and intervals are extracted", {
  results <- data.frame(
    term = c("control", "treatment"),
    estimate = c(0, 1.2),
    conf.low = c(-0.1, 0.4),
    conf.high = c(0.1, 2)
  )
  expect_true(expect_positive_effect(results, "treatment")$passed)
  expect_true(expect_interval_excludes(results, "treatment")$passed)
  expect_false(claimtestR::expect_equivalent(results, c(-0.5, 0.5), "treatment")$passed)
})

test_that("base R fitted models are supported", {
  fit <- lm(mpg ~ wt, data = mtcars)
  direction <- expect_negative_effect(fit, term = "wt")
  interval <- expect_interval_excludes(fit, term = "wt", value = 0)
  expect_true(direction$passed)
  expect_true(interval$passed)
})

test_that("practical effects use absolute magnitude", {
  expect_true(expect_practical_effect(-6, minimum = 5)$passed)
  expect_false(expect_practical_effect(4.9, minimum = 5)$passed)
})

test_that("stability reports the modal direction", {
  stable <- expect_stable_direction(c(rep(1, 19), -1), 0.95)
  unstable <- expect_stable_direction(c(rep(1, 9), -1), 0.95)
  expect_true(stable$passed)
  expect_equal(stable$details$direction, "positive")
  expect_false(unstable$passed)
})

test_that("model improvement respects metric direction", {
  expect_true(expect_model_improves(8, 10, "lower")$passed)
  expect_true(expect_model_improves(0.9, 0.8, "higher", minimum = 0.05)$passed)
  expect_false(expect_model_improves(10, 10, "lower")$passed)
})

test_that("claim suites summarize results", {
  suite <- test_claims(expect_positive_effect(1), expect_negative_effect(1))
  expect_equal(suite$total, 2)
  expect_equal(suite$passed, 1)
  expect_equal(suite$failed, 1)

  nested <- test_claims(list(expect_positive_effect(1), expect_negative_effect(-1)))
  expect_equal(nested$passed, 2)
})

test_that("invalid inputs fail clearly", {
  expect_error(expect_positive_effect(c(1, 2)), "must be")
  expect_error(expect_stable_direction(c(1, NA)), "missing")
  expect_error(claimtestR::expect_equivalent(data.frame(estimate = 1), c(-1, 1)), "interval")
})
