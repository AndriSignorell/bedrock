# Test Whether Values Are (Nearly) Whole Numbers

Checks whether values are integer-like within a numerical tolerance.
Works for numeric, integer, and complex vectors.

## Usage

``` r
isWholeLike(
  x,
  all = TRUE,
  isNonNegative = FALSE,
  tol = sqrt(.Machine$double.eps),
  na.rm = FALSE
)
```

## Arguments

- x:

  A numeric, integer, or complex vector.

- all:

  Logical. If `TRUE` (default), returns a single logical indicating
  whether all elements are whole-like. If `FALSE`, returns a logical
  vector of the same length as `x`.

- isNonNegative:

  Logical. If `TRUE`, additionally requires values to be non-negative.

- tol:

  Numerical tolerance for comparing to the nearest integer. Default is
  `sqrt(.Machine$double.eps)`.

- na.rm:

  Logical. If `TRUE`, missing values are removed before testing. If
  `FALSE` (default) and `x` contains `NA`, the result is `FALSE`.

## Value

If `all = TRUE`, a single logical value. If `all = FALSE`, a logical
vector.

## Details

A value is considered whole-like if the absolute difference between the
value and its nearest integer is smaller than `tol`.

For complex numbers, both real and imaginary parts must be whole-like.

## See also

Other data.inspection: [`allDuplicated()`](allDuplicated.md),
[`allIdentical()`](allIdentical.md),
[`completeColumns()`](completeColumns.md),
[`countCompCases()`](countCompCases.md), [`flags()`](flags.md),
[`isDichotomous()`](isDichotomous.md), [`isEuclid()`](isEuclid.md),
[`isNumeric()`](isNumeric.md), [`isURL()`](isURL.md),
[`isZero()`](isZero.md)

## Examples

``` r
isWholeLike(c(1, 2, 3))
#> [1] TRUE
isWholeLike(c(1, 2.0000001), tol = 1e-6)
#> [1] TRUE
isWholeLike(c(1, 2.5), all = FALSE)
#> [1]  TRUE FALSE
isWholeLike(c(1, -2), isNonNegative = TRUE)
#> [1] FALSE
isWholeLike(1:5 + 0i)
#> [1] TRUE
```
