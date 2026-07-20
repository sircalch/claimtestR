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
  expect_true(leave_one_out$passed)
  expect_length(leave_one_out$details$estimates, nrow(mtcars))
})

test_that("model improvement respects metric direction", {
  expect_true(expect_model_improves(8, 10, "lower")$passed)
  expect_true(expect_model_improves(0.9, 0.8, "higher", minimum = 0.05)$passed)
  expect_false(expect_model_improves(10, 10, "lower")$passed)
})

