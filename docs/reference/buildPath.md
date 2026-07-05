# Construct a normalized file path

Safely constructs a file path from a directory and a filename,
independent of whether the directory ends with a trailing slash. The
resulting path is normalized and uses forward slashes.

## Usage

``` r
buildPath(dir, filename)
```

## Arguments

- dir:

  Character string. Directory path.

- filename:

  Character string. File name to append to `dir`.

## Value

A character string representing the normalized file path.

## Details

This function is a thin wrapper around
[`file.path`](https://rdrr.io/r/base/file.path.html) and
[`normalizePath`](https://rdrr.io/r/base/normalizePath.html). It ensures
consistent path construction across platforms and avoids issues with
trailing slashes.

The argument `mustWork = FALSE` allows returning paths that do not yet
exist.

## Note

Converting between forward slashes and backslashes is a frequent
necessity—and a hassle—especially in Windows. The `FlipSlashes()`
function in the `swissButler` package is useful for this purpose.

## See also

Other file.utils: [`fileExistURL()`](fileExistURL.md),
[`findDownload()`](findDownload.md),
[`parseSASDatalines()`](parseSASDatalines.md),
[`pdfManual()`](pdfManual.md), [`splitPath()`](splitPath.md)

## Examples

``` r
buildPath("data", "file.csv")
#> [1] "C:/temp/bedrock/docs/reference/data/file.csv"
buildPath("data/", "file.csv")
#> [1] "C:/temp/bedrock/docs/reference/data/file.csv"
```
