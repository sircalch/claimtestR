test_that("superiority uses the lower limit when higher is better", {
  x <- data.frame(estimate = 1.4, conf.low = 0.3, conf.high = 2.5)
  expect_true(expect_superior(x)$passed)
  expect_true(expect_superior(x, margin = 0.29)$passed)
  expect_false(expect_superior(x, margin = 0.3)$passed)   # endpoint counts as included
  expect_false(expect_superior(x, margin = 1)$passed)
})

test_that("superiority mirrors onto the upper limit when lower is better", {
  x <- data.frame(estimate = -1.4, conf.low = -2.5, conf.high = -0.3)
  expect_true(expect_superior(x, direction = "lower")$passed)
  expect_false(expect_superior(x, margin = 0.3, direction = "lower")$passed)
  expect_false(expect_superior(x)$passed)
})

test_that("non-inferiority compares the relevant limit with the margin", {
  higher <- data.frame(estimate = 0.1, conf.low = -0.4, conf.high = 0.6)
  expect_true(expect_noninferior(higher, margin = 0.5)$passed)
  expect_false(expect_noninferior(higher, margin = 0.4)$passed)   # boundary
  expect_false(expect_noninferior(higher, margin = 0.3)$passed)

  lower <- data.frame(estimate = -0.02, conf.low = -0.05, conf.high = 0.01)
  expect_true(expect_noninferior(lower, margin = 0.03, direction = "lower")$passed)
  expect_false(expect_noninferior(lower, margin = 0.01, direction = "lower")$passed)
})

test_that("superiority implies non-inferiority for any positive margin", {
  x <- data.frame(estimate = 1, conf.low = 0.2, conf.high = 1.8)
  expect_true(expect_superior(x)$passed)
  for (m in c(0.01, 0.5, 3)) expect_true(expect_noninferior(x, margin = m)$passed)
})

test_that("margins are validated", {
  x <- data.frame(estimate = 1, conf.low = 0.2, conf.high = 1.8)
  expect_error(expect_superior(x, margin = -1), "margin")
  expect_error(expect_noninferior(x, margin = 0), "margin")
  expect_error(expect_noninferior(x, margin = NA_real_), "margin")
  expect_error(expect_superior(x, direction = "sideways"))
})

test_that("interval-based claims require an interval", {
  expect_error(expect_superior(2), "confidence interval")
  expect_error(expect_noninferior(2, margin = 1), "confidence interval")
})

test_that("inferential claims work with lm models and carry details", {
  fit <- lm(mpg ~ wt, data = mtcars)
  res <- expect_superior(fit, term = "wt", direction = "lower")
  ci <- stats::confint(fit, "wt", level = 0.95)
  expect_identical(res$passed, ci[1, 2] < 0)
  expect_equal(res$details$level, 0.95)
  expect_identical(res$claim, "superior")
  res90 <- expect_noninferior(fit, term = "wt", margin = 1, direction = "lower", level = 0.90)
  expect_equal(unname(res90$observed), unname(stats::confint(fit, "wt", level = 0.90)[1, ]))
})
