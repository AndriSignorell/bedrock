## Submission

This is a new submission.

`bedrock` provides the low level utilities used by a family of packages
(the DescToolsX ecosystem) that will be submitted over the coming months.
It has no dependencies on any of them and is fully usable on its own.

## Test environments

* local: Windows 11, R 4.5.1
* win-builder: R-devel, R-release, R-oldrelease
* macbuilder: R-release (macOS, arm64)
* GitHub Actions: ubuntu-latest (R-release, R-devel), macOS-latest, windows-latest

## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Andri Signorell <andri@signorell.net>'

New submission
```

This note is expected for a first submission.

## Notes on the check output

* Examples, tests and the vignette run cleanly. The test suite comprises
  2143 expectations with no failures and no skips; network dependent
  functions are covered via `testthat::local_mocked_bindings()` and do
  not access the internet during checks.
* Examples wrapped in `\dontrun{}` require either a local file, an
  interactive viewer, or network access, and cannot be executed
  non-interactively.

## Downstream dependencies

There are currently no downstream dependencies on CRAN.
