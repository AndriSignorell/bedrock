# Back to Basics with Tibbles

Sometimes we might wish for the old days be back and want to work with
familiar objects. This function helps to convert `tibbles` to
`data.frames` as smoothly as possible.

## Usage

``` r
toBaseR(x, ...)

# S3 method for class 'tbl_df'
toBaseR(x, ...)

# S3 method for class 'haven_labelled'
toBaseR(x, ...)

# Default S3 method
toBaseR(x, ...)
```

## Arguments

- x:

  the object to be converted.

- ...:

  arguments passed on.

## Value

converted object.

## See also

Other data.coerce: [`as.array.xtabs()`](as.array.xtabs.md),
[`type-aliases`](type-aliases.md)

## Examples

``` r
# a tibble is rolled back to a plain data.frame
if (requireNamespace("tibble", quietly = TRUE)) {
  tbl <- tibble::as_tibble(head(iris))
  class(toBaseR(tbl))
}
#> [1] "data.frame"

# an object without a method is returned unchanged, with a warning
x <- suppressWarnings(toBaseR(1:3))
identical(x, 1:3)
#> [1] TRUE

# \donttest{
# labelled data from other statistical packages: needs 'haven' and
# an internet connection, hence the try()
if (requireNamespace("haven", quietly = TRUE)) {
  url <- "http://www.stata.com/videos13/data/webclass.dta"
  d.webclass <- try(toBaseR(haven::read_dta(url)))
}
# }
```
