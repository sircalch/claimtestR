test_that("expect_claim_passes bridges claims to testthat", {
  passing <- expect_positive_effect(1)
  failing <- expect_positive_effect(-1)

  expect_invisible(expect_claim_passes(passing))
  expect_failure(expect_claim_passes(failing), "positive")
  expect_error(expect_claim_passes(TRUE), "claim_test")
})
