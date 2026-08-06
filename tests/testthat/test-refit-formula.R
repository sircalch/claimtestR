make_local_formula_lm <- function() {
  model_data <- mtcars
  stored_formula <- mpg ~ log(wt) * factor(am)
  lm(stored_formula, data = model_data)
}

make_local_formula_glm <- function() {
  model_data <- mtcars
  stored_formula <- am ~ log(wt)
  stored_family <- binomial()
  glm(stored_formula, data = model_data, family = stored_family)
}

test_that("stored local formula objects are used for lm refits", {
  fit <- make_local_formula_lm()

  bootstrap <- expect_stable_direction(
    fit, term = "log(wt)", method = "bootstrap",
    iterations = 10, seed = 12, minimum_proportion = 0
  )
  leave_one_out <- expect_stable_direction(
    fit, term = "log(wt)", method = "leave_one_out",
    minimum_proportion = 0
  )

  expect_true(bootstrap$details$resampling_valid)
  expect_identical(bootstrap$details$failed, 0L)
  expect_true(leave_one_out$details$resampling_valid)
  expect_identical(leave_one_out$details$failed, 0L)
  expect_false(any(grepl("stored_formula", bootstrap$details$failures$message)))
})

test_that("stored local formulas and families are used for glm refits", {
  fit <- make_local_formula_glm()

  bootstrap <- expect_stable_direction(
    fit, term = "log(wt)", method = "bootstrap",
    iterations = 10, seed = 123, minimum_proportion = 0
  )
  leave_one_out <- expect_stable_direction(
    fit, term = "log(wt)", method = "leave_one_out",
    minimum_proportion = 0
  )

  expect_true(bootstrap$details$successful >= 8L)
  expect_true(bootstrap$details$resampling_valid)
  expect_true(leave_one_out$details$resampling_valid)
  expect_false(any(grepl(
    "stored_formula|stored_family", leave_one_out$details$failures$message
  )))
})

test_that("serialized local-formula lm and glm models remain refittable", {
  fits <- list(
    lm = list(make_local_formula_lm(), "log(wt)"),
    glm = list(make_local_formula_glm(), "log(wt)")
  )

  for (name in names(fits)) {
    path <- tempfile(fileext = ".rds")
    saveRDS(fits[[name]][[1]], path)
    reloaded <- readRDS(path)
    unlink(path)

    result <- expect_stable_direction(
      reloaded, term = fits[[name]][[2]], method = "leave_one_out",
      minimum_proportion = 0
    )
    expect_true(result$details$resampling_valid, info = name)
    expect_identical(result$details$failed, 0L, info = name)
  }
})
