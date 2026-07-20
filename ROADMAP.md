# claimtestR roadmap

`claimtestR` is the first implementation in a planned research-software line on
the reliability of scientific analyses. The public API will evolve through
small, reviewable releases.

## 0.1.0 — Core claims

- [x] Consistent `claim_test` and `claim_test_suite` objects.
- [x] Positive and negative direction claims.
- [x] Interval inclusion and exclusion.
- [x] Practical magnitude and equivalence.
- [x] Numeric, matrix, data-frame, `lm`, and `glm` inputs.
- [x] testthat bridge and Markdown reporting.
- [x] Basic bootstrap and leave-one-out direction stability.

## 0.2.0 — Extraction ecosystem

Tracking: [issue #1](https://github.com/sircalch/claimtestR/issues/1)

- [ ] Public extension contract for new model classes.
- [ ] Optional integration tests with broom.
- [ ] Support for `lmerMod` and related mixed-model objects.
- [ ] Better handling of transformed formulas and resampling strata.

## 0.3.0 — Inferential claims

Tracking: [issue #2](https://github.com/sircalch/claimtestR/issues/2)

- [ ] Non-inferiority and superiority claims.
- [ ] Configurable threshold registries.
- [ ] Model-selection and ranking claims.
- [ ] Formal decision-distance summaries.

## 0.4.0 — Stability

Tracking: [issue #3](https://github.com/sircalch/claimtestR/issues/3)

- [ ] Subsampling and cluster bootstrap.
- [ ] Stability of significance and practical relevance.
- [ ] Resampling diagnostics and failure accounting.
- [ ] Parallel execution with reproducible random-number streams.

## 0.5.0 — Workflow integration

Tracking: [issue #4](https://github.com/sircalch/claimtestR/issues/4)

- [ ] JSON export with a versioned schema.
- [ ] Quarto and R Markdown templates.
- [ ] Richer GitHub Actions examples.
- [ ] Machine-readable audit artifacts.

## 1.0.0 — Stable release

Tracking: [issue #5](https://github.com/sircalch/claimtestR/issues/5)

- [ ] External API and statistical-method review.
- [ ] High test coverage across supported R versions.
- [ ] CRAN release and archived Zenodo release.
- [ ] JOSS software paper.

## Related packages

[`invariantR`](https://github.com/sircalch/invariantR) remains a reserved
follow-on project. Development should begin only after the central claim object
and extension API in `claimtestR` have stabilized. No additional package
repositories are planned before that point.
