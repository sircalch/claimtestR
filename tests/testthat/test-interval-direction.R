test_that("direction claims return passing and failing results", {
  expect_true(expect_positive_effect(2)$passed)
  expect_false(expect_positive_effect(-2)$passed)
  expect_true(expect_negative_effect(-2)$passed)
  expect_false(expect_negative_effect(0)$passed)
})

test_that("interval endpoints count as included", {
  result <- data.frame(estimate = 0.5, conf.low = 0, conf.high = 1)
  expect_true(expect_interval_includes(result, value = 0)$passed)
  expect_false(expect_interval_excludes(result, value = 0)$passed)
  expect_true(expect_interval_excludes(result, value = 2)$passed)
})

test_that("practical effect direction is configurable", {
  expect_true(expect_practical_effect(-6, minimum = 5)$passed)
  expect_true(expect_practical_effect(-6, minimum = 5, direction = "negative")$passed)
  expect_false(expect_practical_effect(-6, minimum = 5, direction = "positive")$passed)
  expect_error(expect_practical_effect(1, minimum = -1), "minimum")
})

test_that("equivalence requires the complete interval inside the bounds", {
  inside <- data.frame(estimate = 0, conf.low = -0.2, conf.high = 0.2)
  outside <- data.frame(estimate = 0, conf.low = -0.6, conf.high = 0.2)
  boundary <- data.frame(estimate = 0, conf.low = -0.5, conf.high = 0.5)

  expect_true(claimtestR::expect_equivalent(inside, c(-0.5, 0.5))$passed)
  expect_false(claimtestR::expect_equivalent(outside, c(-0.5, 0.5))$passed)
  expect_true(claimtestR::expect_equivalent(boundary, c(-0.5, 0.5))$passed)
  expect_error(claimtestR::expect_equivalent(inside, c(1, -1)), "increasing order")
})

