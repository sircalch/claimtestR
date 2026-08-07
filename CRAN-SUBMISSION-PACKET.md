# CRAN submission packet — claimtestR 0.1.1

Use this file only when the CRAN submission form reopens. The source archive is
already public and must not be rebuilt or edited before upload.

## Upload file

- File: `claimtestR_0.1.1.tar.gz`
- Version: 0.1.1
- GitHub release commit: `85cf6d74258d98c36daaf52fc9a63b28beaf8a86`
- SHA-256:
  `33468D21865E847B253AFB4BF886292D01E168A801843B21650384F3B93AAE25`
- Zenodo version DOI: https://doi.org/10.5281/zenodo.21833717

## Submission text

This is a new package submission. The package makes no network calls, writes
no files, and has no compiled code. Examples and tests use only small,
in-memory objects.

The package was checked on Windows 11 with R 4.6.1 using `R CMD check
--as-cran`: 0 errors, 0 warnings, and one expected NOTE for a new submission.
GitHub Actions also completed successfully on Linux (R release, oldrel, and
devel), Windows, and macOS.

## Maintainer actions at submission

1. Upload exactly `claimtestR_0.1.1.tar.gz` using CRAN's official form.
2. Use the maintainer identity in `DESCRIPTION`; do not change version or
   metadata during the upload.
3. Paste the submission text above if the form asks for comments.
4. Open the confirmation email delivered to `andres.monreal@ues.mx` and confirm
   the submission promptly.
5. Preserve any reply from CRAN and respond factually, with a patch only if
   requested by the CRAN reviewer.

## Do not do before CRAN responds

- Do not overwrite the GitHub `v0.1.1` release.
- Do not replace the Zenodo record or its archived file.
- Do not claim that the package is on CRAN until CRAN accepts it.
- Do not make a 0.1.1 code change. A requested correction should become a new,
  separately validated release.
