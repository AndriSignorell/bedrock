# Check Whether a String Is a File Path

Returns `TRUE` if the given string looks like a local file path
(absolute or relative, Unix/Windows style), `FALSE` otherwise.
Convenience wrapper around the internal `.detectInputType()` helper.

## Usage

``` r
isFilePath(x)
```

## Arguments

- x:

  `character(1)` - the string to test.

## Value

`logical(1)` - `TRUE` if `x` is a file path, `FALSE` otherwise.

## See also

[`isURL()`](isURL.md) for the complementary URL check.

Other file.path: [`buildPath()`](buildPath.md),
[`fileExistURL()`](fileExistURL.md),
[`findDownload()`](findDownload.md), [`isURL()`](isURL.md),
[`splitPath()`](splitPath.md)

## Examples

``` r
isFilePath("/home/user/data/file.csv")   # TRUE
#> [1] TRUE
isFilePath("~/documents/report.pdf")     # TRUE
#> [1] TRUE
isFilePath("./relative/path/file.R")     # TRUE
#> [1] TRUE
isFilePath("../other/folder/data.rds")   # TRUE
#> [1] TRUE
isFilePath("C:/Users/Hans/file.xlsx")    # TRUE
#> [1] TRUE
isFilePath("https://example.com/f.csv")  # FALSE
#> [1] FALSE
```
