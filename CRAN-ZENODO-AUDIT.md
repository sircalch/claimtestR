# CRAN and Zenodo readiness audit

Audit date: 2026-08-07 UTC. Scope: local source tree only; no CRAN submission,
GitHub push, Zenodo publication, or release was performed.

## Findings

1. The source tree is version 0.1.1 and has MIT licensing, documentation,
   tests, a vignette, and a CRAN comments file.
2. The previously built `claimtestR_0.1.0.tar.gz` differs from the current
   source metadata: its `DESCRIPTION` includes the retired pkgdown URL
   `https://sircalch.github.io/claimtestR/`, which returned 404 during the
   earlier incoming-feasibility check. The current source `DESCRIPTION` no
   longer contains that URL. Do not submit the old tarball.
3. The local `R CMD check --as-cran` of the old tarball stopped because the
   invoking R session did not use the project library containing `knitr`,
   `rmarkdown`, and `testthat`. This was an environment issue, not a package
   defect.
4. The current source was rebuilt with its vignette, then checked from a local
   temporary directory outside OneDrive to avoid a Windows file-lock during
   staged installation. On Windows 11 with R 4.6.1, the resulting 0.1.1 tarball
   passed `R CMD check --as-cran` with 0 errors, 0 warnings, and only the
   expected new-submission NOTE.
5. The maintainer field used a GitHub no-reply address. It has been updated in
   the source tree to the corresponding author's institutional email and ORCID.
6. Zenodo metadata was absent. A draft `.zenodo.json` now describes the
   released software; it will be consumed only after the repository is enabled
   in Zenodo and a future GitHub release is created.

## Required gates before CRAN

- Repeat `R CMD build .` and `R CMD check --as-cran` on a second platform,
  preferably Linux, before submission.
- Retain the current Windows result (0 errors, 0 warnings, one expected NOTE)
  with the submission materials.
- Check all URLs in the new tarball and the rendered vignette.
- Update `cran-comments.md` with the actual platform, R version, date, and
  final check result.
- Do not reuse the public GitHub 0.1.0 archive; the CRAN candidate is version
  0.1.1 because its distribution metadata differs.
- Obtain the maintainer's explicit authorization immediately before upload.

## Zenodo publication gate

- Enable `sircalch/claimtestR` in Zenodo's GitHub integration.
- Review the imported creators, version, license, title, description, and
  keywords before publication.
- Create a GitHub release only after the CRAN-ready source and release notes
  have been approved.
- Record the version DOI and the concept DOI in `CITATION.cff`, README, package
  documentation, and the future R Journal manuscript.

## Relationship to the article

The R Journal's current pre-submission checks include package availability on
CRAN or Bioconductor. The software should therefore complete its CRAN route
before that manuscript is submitted.
