test_that("claim results have a consistent public structure", {
  claim <- claim_result(
    TRUE, "positive_effect", 4.2, "> 0", term = "treatment",
    message = "The treatment effect is positive.", evidence = "estimate = 4.2"
  )

  expect_s3_class(claim, "claim_test")
  expect_named(
    claim,
    c("passed", "claim", "observed", "expected", "term", "message",
      "evidence", "details", "call")
  )
  expect_identical(claim$claim, "positive_effect")
  expect_identical(claim$term, "treatment")
  expect_match(paste(capture.output(print(claim)), collapse = "\n"), "Status: PASS")

  row <- as.data.frame(claim)
  expect_identical(row$status, "PASS")
  expect_identical(row$evidence, "estimate = 4.2")
})

test_that("claim result validation is explicit", {
  expect_error(claim_result(NA, "positive_effect"), "passed")
  expect_error(claim_result(TRUE, "Positive effect"), "snake_case")
  expect_error(claim_result(TRUE, "positive_effect", term = ""), "term")
  expect_error(claim_result(TRUE, "positive_effect", message = ""), "message")
  expect_error(claim_result(TRUE, "positive_effect", details = 1), "details")
})
