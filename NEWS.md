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
- Fixed pre-release model refits so transformed responses and predictors,
  interactions, `I()`, and `poly()` are reevaluated from the original data
  after aligning to the observations retained by the fitted model.
- Hardened pre-release data recovery: stability refits now accept only a simple
  name bound to an ordinary base data frame, reject active bindings and all
  data-producing calls, and reject custom S3 or S4 columns before generic
  methods can be dispatched.
- Refit from the formula object retained by `lm` or `glm`, so local formula and
  family symbols need not remain available after fitting or serialization.
- Rejects non-converged `glm` objects and records failed or non-converged
  resample refits with explicit validity diagnostics.
- Added `expect_claim_passes()` as an optional bridge to testthat.
- Added suite data-frame conversion and dependency-free Markdown reporting.
- Added a complete getting-started vignette and expanded validation tests.
