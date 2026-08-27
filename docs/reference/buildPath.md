# Construct a Normalized File Path

Safely constructs a file path from a directory and a filename,
independent of whether the directory ends with a trailing slash. The
resulting path uses forward slashes.

## Usage

``` r
buildPath(dir, filename)
```

## Arguments

- dir:

  character string. Directory path.

- filename:

  character string. File name to append to `dir`.

## Value

a character string representing the file path.

## Details

Trailing slashes (or backslashes) in `dir` are removed before the
components are joined, so `buildPath("data", "file.csv")` and
`buildPath("data/", "file.csv")` yield the same result.

The path is then passed through
[`normalizePath`](https://rdrr.io/r/base/normalizePath.html) with
`mustWork = FALSE`, so paths that do not (yet) exist are allowed. Note
that `normalizePath` resolves existing paths to absolute form, while
non-existing paths are returned as constructed (i.e. possibly relative).

Both arguments are vectorized in the usual
[`file.path`](https://rdrr.io/r/base/file.path.html) manner.

## Note

Converting between forward slashes and backslashes is a frequent
necessity – and a hassle – especially in Windows. The `cycleSlashes()`
function in the `swissValet` package is useful for this purpose.

## See also

Other file.path: [`fileExistURL()`](fileExistURL.md),
[`findDownload()`](findDownload.md), [`isFilePath()`](isFilePath.md),
[`isURL()`](isURL.md), [`splitPath()`](splitPath.md)

## Examples

``` r
buildPath("data", "file.csv")
#> [1] "C:/temp/bedrock/docs/reference/data/file.csv"
buildPath("data/", "file.csv")
#> [1] "C:/temp/bedrock/docs/reference/data/file.csv"
```
