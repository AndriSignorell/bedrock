# Check Whether a String Is a URL

Returns `TRUE` if the given string starts with a recognised URL scheme,
`FALSE` otherwise. Convenience wrapper around the internal
`.detectInputType()` helper.

## Usage

``` r
isUrl(x)
```

## Arguments

- x:

  `character(1)` - the string to test.

## Value

`logical(1)` - `TRUE` if `x` is a URL, `FALSE` otherwise.

## See also

For the complementary check on an existing path, see
[`isFilePath()`](isFilePath.md).

Other file.path: [`buildPath()`](buildPath.md),
[`findDownload()`](findDownload.md), [`isFilePath()`](isFilePath.md),
[`splitPath()`](splitPath.md), [`urlExists()`](urlExists.md)

## Examples

``` r
isUrl("https://example.com/data.csv")   # TRUE
#> [1] TRUE
isUrl("ftp://files.example.org/x.zip")  # TRUE
#> [1] TRUE
isUrl("s3://my-bucket/file.parquet")    # TRUE
#> [1] TRUE
isUrl("/home/user/file.csv")            # FALSE
#> [1] FALSE
isUrl("./script.R")                     # FALSE
#> [1] FALSE
```
