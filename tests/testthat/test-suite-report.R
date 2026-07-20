test_that("claim suites flatten, summarize, and tabulate results", {
  suite <- test_claims(
    expect_positive_effect(1),
    list(expect_negative_effect(-1), list(expect_positive_effect(-1)))
  )
  expect_s3_class(suite, "claim_test_suite")
  expect_equal(suite$total, 3)
  expect_equal(suite$passed, 2)
  expect_equal(suite$failed, 1)
  expect_identical(summary(suite), suite)
  expect_equal(nrow(as.data.frame(suite)), 3)
  expect_match(paste(capture.output(print(suite)), collapse = "\n"),
               "66.7% pass rate", fixed = TRUE)
})

test_that("claim suites reject empty or mixed inputs", {
  expect_error(test_claims(), "at least one")
  expect_error(test_claims(expect_positive_effect(1), 3), "Every input")
})

test_that("report_claims produces data frames and Markdown", {
  suite <- test_claims(expect_positive_effect(1), expect_negative_effect(1))
  data <- report_claims(suite, "data.frame")
  markdown <- report_claims(suite, "markdown")

  expect_s3_class(data, "data.frame")
  expect_s3_class(markdown, "claim_report")
  expect_match(as.character(markdown), "| Claim | Result | Evidence |", fixed = TRUE)
  expect_match(paste(capture.output(print(markdown)), collapse = "\n"), "Passed")
  expect_error(report_claims(1), "claim test")
})
