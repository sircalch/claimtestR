# claimtestR

<!-- badges: start -->
[![R-CMD-check](https://github.com/sircalch/claimtestR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sircalch/claimtestR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`claimtestR` turns scientific claims into executable, inspectable assertions.
It is designed for reproducible reports, analysis pipelines, and regression
testing of statistical conclusions.

## Why?

Ordinary software tests ask whether an object has the expected value.
Scientific workflows also need to ask whether the conclusion still holds:

- Is the estimated effect positive?
- Does its interval exclude zero?
- Is the magnitude practically relevant?
- Does the conclusion retain its direction under sensitivity analyses?
- Does a candidate model actually improve on a baseline?

## Installation

This is an early development version. Once the repository is published, install
it with:

```r
# install.packages("remotes")
remotes::install_github("sircalch/claimtestR")
```

## Example

```r
library(claimtestR)

results <- data.frame(
  term = "treatment",
  estimate = 4.2,
  conf.low = 1.1,
  conf.high = 7.3
)

claims <- test_claims(
  expect_positive_effect(results, term = "treatment"),
  expect_interval_excludes(results, term = "treatment", value = 0),
  expect_practical_effect(results, term = "treatment", minimum = 5)
)

claims
#> Claim tests: 3 total | 2 passed | 1 failed
```

Each assertion returns a `claim_test` object instead of stopping execution. This
makes failures visible and allows a complete audit to be reported. Integrations
that turn failed claims into CI errors can be layered on top of this stable core.

`testthat` also exports a function named `expect_equivalent()`. When both
packages are attached, use `claimtestR::expect_equivalent()` to make the intended
function explicit.

## Initial scope

The first version intentionally uses claims explicitly declared by the analyst.
It does not infer scientific meaning from prose and does not claim to choose an
appropriate statistical method automatically.

## Development

```r
devtools::document()
devtools::test()
devtools::check()
```

## License

MIT © 2026 Andres Monreal.
