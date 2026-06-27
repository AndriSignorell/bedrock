# Check whether a string is a URL

Returns `TRUE` if the given string starts with a recognised URL scheme,
`FALSE` otherwise. Convenience wrapper around the internal
`.detectInputType()` helper.

## Usage

``` r
isURL(x)
```

## Arguments

- x:

  `character(1)` – the string to test.

## Value

`logical(1)` – `TRUE` if `x` is a URL, `FALSE` otherwise.

## See also

[`isFilePath()`](isFilePath.md) for the complementary file-path check.

## Examples

``` r
isURL("https://example.com/data.csv")   # TRUE
#> [1] TRUE
isURL("ftp://files.example.org/x.zip")  # TRUE
#> [1] TRUE
isURL("s3://my-bucket/file.parquet")    # TRUE
#> [1] TRUE
isURL("/home/user/file.csv")            # FALSE
#> [1] FALSE
isURL("./script.R")                     # FALSE
#> [1] FALSE
```
