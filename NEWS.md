# claimtestR 0.1.0

## First public core

- Formalized `claim_test` results with machine-readable identifiers, terms,
  messages, evidence, expectations, and diagnostic details.
- Added interval inclusion and directional practical-effect assertions.
- Added an extensible internal extraction interface for numeric vectors,
  matrices, data frames, `lm`, and `glm` objects. This also accepts
  `broom::tidy()`-style data frames without requiring broom.
- Added bootstrap and leave-one-out direction-stability checks for `lm` and
  `glm` models.
- Rejects non-converged `glm` objects and records failed or non-converged
  resample refits with explicit validity diagnostics.
- Added `expect_claim_passes()` as an optional bridge to testthat.
- Added suite data-frame conversion and dependency-free Markdown reporting.
- Added a complete getting-started vignette and expanded validation tests.
