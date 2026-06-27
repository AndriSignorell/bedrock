# Identify Columns Without Missing Values

This function checks each element of a data frame or list-like object
for missing values (`NA`) and identifies those that are completely
observed, i.e., contain no missing entries.

## Usage

``` r
completeColumns(x, which = TRUE)
```

## Arguments

- x:

  A data.frame or list-like object whose elements are checked for
  missing values.

- which:

  Logical. If `TRUE` (default), the function returns the names of
  elements without missing values. If `FALSE`, a logical vector is
  returned, where each element corresponds to an element in `x` and
  indicates whether it is complete.

## Value

If `which = TRUE`, a character vector with the names of all complete
elements.

If `which = FALSE`, a logical vector of length `length(x)`, where `TRUE`
indicates that the corresponding element contains no missing values.

## Details

Depending on the argument `which`, the function either returns the names
of such elements or a logical vector indicating completeness for each
element.

An element is considered *complete* if it contains zero missing values.
Internally, the function uses [`anyNA`](https://rdrr.io/r/base/NA.html)
to detect missing values.

## See also

[`anyNA`](https://rdrr.io/r/base/NA.html),
[`is.na`](https://rdrr.io/r/base/NA.html),
[`na.omit`](https://rdrr.io/r/stats/na.fail.html),
[`complete.cases`](https://rdrr.io/r/stats/complete.cases.html)

Other data.inspection: [`allDuplicated()`](allDuplicated.md),
[`allIdentical()`](allIdentical.md),
[`countCompCases()`](countCompCases.md), [`flags()`](flags.md),
[`isDichotomous()`](isDichotomous.md), [`isNumeric()`](isNumeric.md),
[`isWholeLike()`](isWholeLike.md), [`isZero()`](isZero.md)

## Examples

``` r
# Names of columns without missing values
completeColumns(Pizza)
#> [1] "index"        "delivery_min"

# Logical vector indicating completeness
completeColumns(Pizza, which = FALSE)
#>          index           date           week        weekday           area 
#>           TRUE          FALSE          FALSE          FALSE          FALSE 
#>          count         rabate          price       operator         driver 
#>          FALSE          FALSE          FALSE          FALSE          FALSE 
#>   delivery_min    temperature   wine_ordered wine_delivered     wrongpizza 
#>           TRUE          FALSE          FALSE          FALSE          FALSE 
#>        quality     vegetarian            nps      complaint          style 
#>          FALSE          FALSE          FALSE          FALSE          FALSE 
#>        channel            tip 
#>          FALSE          FALSE 
```
