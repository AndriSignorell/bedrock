# Back to Basics with Tibbles and data.tables

Sometimes we might wish for the old days be back and want to work with
familiar objects. This function helps to convert `tibbles` and
`data.tables` to plain `data.frames` as smoothly as possible.

## Usage

``` r
toBaseR(x, ...)

# S3 method for class 'tbl_df'
toBaseR(x, ...)

# S3 method for class 'data.table'
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

## Details

For a `data.table` the key (`"sorted"`), secondary indices (`"index"`)
and the `".internal.selfref"` pointer are dropped, and a deep copy is
returned, so that later modifications by reference on the original
cannot leak into the result. Columns of class `haven_labelled` are
converted to factors in both cases.

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

# same for a keyed data.table
dt <- data.table::as.data.table(head(iris))
data.table::setkey(dt, Species)
df <- toBaseR(dt)
class(df)
#> [1] "data.frame"
attr(df, "sorted")
#> NULL

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
