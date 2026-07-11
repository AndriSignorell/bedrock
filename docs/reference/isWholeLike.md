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

  a numeric, integer, or complex vector

- all:

  logical. If `TRUE` (default), returns a single logical indicating
  whether all elements are whole-like. If `FALSE`, returns a logical
  vector of the same length as `x`.

- isNonNegative:

  logical. If `TRUE`, additionally requires values to be non-negative.

- tol:

  numerical tolerance for comparing to the nearest integer. Default is
  `sqrt(.Machine$double.eps)`.

- na.rm:

  logical. If `TRUE`, missing values are removed before testing. If
  `FALSE` (default) and `x` contains `NA`, the result is `FALSE`.

## Value

If `all = TRUE`, a single logical value. If `all = FALSE`, a logical
vector.

## Details

A value is considered whole-like if the absolute difference between the
value and its nearest integer is smaller than `tol`.

For complex numbers, both real and imaginary parts must be whole-like;
with `isNonNegative = TRUE`, both parts must additionally be
non-negative.

## See also

Other data.predicate: [`flags()`](flags.md),
[`isDichotomous()`](isDichotomous.md), [`isEuclid()`](isEuclid.md),
[`isLowCardinality()`](isLowCardinality.md),
[`isNumeric()`](isNumeric.md), [`isZero()`](isZero.md),
[`nUnique()`](nUnique.md)

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
