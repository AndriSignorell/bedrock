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

if (FALSE) { # \dontrun{
# read a Stata file
url <- "http://www.stata.com/videos13/data/webclass.dta"
d.webclass <- toBaseR(haven::read_dta(url))

# read a SPSS file
url <- "https://stats.idre.ucla.edu/wp-content/uploads/2020/10/missing.sav"
d.miss <- toBaseR(haven::read_sav(url))
} # }
```
