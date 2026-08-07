# CRAN and Zenodo release record

Audit date: 2026-08-07 UTC. This document records the state of the public
`claimtestR` 0.1.1 release. It is not part of the CRAN source bundle.

## Immutable release artefacts

- GitHub release: `v0.1.1`, commit
  `85cf6d74258d98c36daaf52fc9a63b28beaf8a86`.
- Candidate source bundle: `claimtestR_0.1.1.tar.gz`.
- SHA-256:
  `33468D21865E847B253AFB4BF886292D01E168A801843B21650384F3B93AAE25`.
- Zenodo version record: https://doi.org/10.5281/zenodo.21833717.
- Zenodo file MD5: `c207859457f1fdfc439b573bb8b52dbb`.

The older 0.1.0 tarball is not a CRAN candidate. Its distribution metadata
contains a retired pkgdown URL.

## Completed validation

1. The source bundle was built with its vignette on Windows 11 using R 4.6.1.
2. `R CMD check --as-cran` was run in a temporary directory outside OneDrive to
   avoid Windows file locks. The result was 0 errors, 0 warnings, and one
   expected NOTE for a new submission.
3. GitHub Actions completed on Linux (R release, oldrel, and devel), Windows,
   and macOS; pkgdown and coverage checks also completed successfully.
4. The package has MIT licensing, documentation, tests, a vignette, and a
   maintainer address at Universidad Estatal de Sonora.
5. The Zenodo record is public and GitHub-Zenodo integration is enabled for
   future releases. This record was uploaded manually because the integration
   was enabled after `v0.1.1` had been created.

## Remaining action

Submit the immutable 0.1.1 source bundle through CRAN's official submission
form once it reopens. As of 2026-08-07, CRAN has suspended submissions from
2026-08-05 through 2026-08-19 for maintenance and team vacation. The
maintainer must complete the confirmation received at the address recorded in
`DESCRIPTION` after upload.

## Relationship to the article

The R Journal's current pre-submission checks include package availability on
CRAN or Bioconductor. The software should therefore complete its CRAN route
before that manuscript is submitted.
