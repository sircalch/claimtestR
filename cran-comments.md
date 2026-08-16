## R CMD check results

Tested locally on Windows 11 with R 4.6.1 using `--as-cran` on 2026-08-16:

```text
0 errors | 0 warnings | 1 note
```

The only note is the expected "New submission" note for a package that has not
yet been published on CRAN.

## Submission notes

This is a new package submission. The package makes no network calls, writes no
files, and has no compiled code. Examples and tests use only small in-memory
objects.
