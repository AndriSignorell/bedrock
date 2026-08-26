# Check Whether a String Is a URL

Returns `TRUE` if the given string starts with a recognised URL scheme,
`FALSE` otherwise. Convenience wrapper around the internal
`.detectInputType()` helper.

## Usage

``` r
isURL(x)
```

## Arguments

- x:

  `character(1)` - the string to test.

## Value

`logical(1)` - `TRUE` if `x` is a URL, `FALSE` otherwise.

## See also

[`isFilePath()`](https://andrisignorell.github.io/bedrock/reference/isFilePath.md)
for the complementary file-path check.

Other file.path:
[`buildPath()`](https://andrisignorell.github.io/bedrock/reference/buildPath.md),
[`fileExistURL()`](https://andrisignorell.github.io/bedrock/reference/fileExistURL.md),
[`findDownload()`](https://andrisignorell.github.io/bedrock/reference/findDownload.md),
[`isFilePath()`](https://andrisignorell.github.io/bedrock/reference/isFilePath.md),
[`splitPath()`](https://andrisignorell.github.io/bedrock/reference/splitPath.md)

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
