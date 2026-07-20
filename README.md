# claimtestR

<!-- badges: start -->
[![R-CMD-check](https://github.com/sircalch/claimtestR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sircalch/claimtestR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`claimtestR` turns scientific claims into executable, inspectable assertions.
It is designed for reproducible reports, analysis pipelines, and regression
testing of statistical conclusions.

The current release is **0.1.0**, the first public core API.

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

## Supported inputs

Assertions accept numeric estimates, matrices, result data frames (including
the standard columns returned by `broom::tidy()`), and base R `lm` and `glm`
models. Model terms are selected explicitly:

```r
model <- lm(mpg ~ wt + hp, data = mtcars)

expect_negative_effect(model, term = "wt")
expect_interval_excludes(model, term = "wt", value = 0)
expect_practical_effect(model, term = "wt", minimum = 2, direction = "negative")
```

`glm` objects must have `converged = TRUE`. claimtestR stops with an actionable
error for a non-converged model rather than treating its coefficients or
intervals as reliable evidence. It does not attempt to repair or reinterpret
the fit.

## Resampling diagnostics

Bootstrap and leave-one-out stability checks retain every attempted refit.
Failed refits count against the attempted-resample denominator and are never
silently removed. A model-based stability assessment is valid only when at
least two refits and at least 80% of all attempted refits succeed. Otherwise,
the function returns a failed `claim_test` explaining that stability could not
be assessed reliably.

The returned `details` field includes `attempted`, `successful`, `failed`,
`success_proportion`, `minimum_success_proportion`, `resampling_valid`, the
full estimate vector with `NA` at failed attempts, and data frames containing
failure classes/messages and captured warnings. Bootstrap sampling remains
reproducible with `seed`, while the caller's random-number state is restored on
exit.

## Continuous integration and reports

Use `expect_claim_passes()` inside testthat to turn a scientific conclusion into
a CI requirement:

```r
testthat::test_that("the primary conclusion remains valid", {
  expect_claim_passes(expect_negative_effect(model, term = "wt"))
})
```

For Quarto or R Markdown, `report_claims()` produces a Markdown table, while
`as.data.frame()` returns data suitable for `knitr::kable()` or export.

## Initial scope

The package intentionally uses claims explicitly declared by the analyst.
It does not infer scientific meaning from prose and does not claim to choose an
appropriate statistical method automatically.

See `vignette("getting-started", package = "claimtestR")` for an end-to-end
workflow and [ROADMAP.md](ROADMAP.md) for planned releases.

## Development

```r
devtools::document()
devtools::test()
devtools::check()
```

## License

MIT © 2026 Andres Monreal.
