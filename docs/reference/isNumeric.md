# Check Whether an Object Is a Valid Numeric Vector

Validates that an object is numeric and optionally satisfies additional
structural constraints such as integer-valuedness or positivity.

## Usage

``` r
isNumeric(
  x,
  isIntegerValued = FALSE,
  isPositive = FALSE,
  tol = sqrt(.Machine$double.eps),
  na.rm = FALSE
)
```

## Arguments

- x:

  an object to be tested

- isIntegerValued:

  logical. If `TRUE`, values must be whole-like (within tolerance). Uses
  [`isWholeLike`](isWholeLike.md) internally.

- isPositive:

  logical. If `TRUE`, all values must be strictly greater than zero.

- tol:

  numerical tolerance used when `isIntegerValued = TRUE`. Default is
  `sqrt(.Machine$double.eps)`.

- na.rm:

  logical. If `TRUE`, missing values are removed before validation. If
  `FALSE` (default) and `x` contains `NA`, the function returns `FALSE`.

## Value

A single logical value.

## Details

The function checks:

- Whether `x` is numeric.

- Whether all values are finite.

- Optional integer-like constraint via
  [`isWholeLike()`](isWholeLike.md).

- Optional positivity constraint.

This function is intended for internal validation in statistical
routines. Length validation is the responsibility of the caller and
should be performed separately with an explicit
[`length()`](https://rdrr.io/r/base/length.html) check.

## See also

[`isWholeLike`](isWholeLike.md)

Other data.predicate: [`flags()`](flags.md),
[`isDichotomous()`](isDichotomous.md), [`isEuclid()`](isEuclid.md),
[`isLowCardinality()`](isLowCardinality.md),
[`isWholeLike()`](isWholeLike.md), [`isZero()`](isZero.md),
[`nUnique()`](nUnique.md)

## Examples

``` r
isNumeric(c(1, 2, 3))
#> [1] TRUE
isNumeric(c(1, 2.1, 3), isIntegerValued = TRUE)
#> [1] FALSE
isNumeric(c(1, -2, 3), isPositive = TRUE)
#> [1] FALSE
isNumeric(c(1, NA), na.rm = TRUE)
#> [1] TRUE
```
