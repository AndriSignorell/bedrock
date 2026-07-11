# Random Numbers Summing to 1

Generates a vector of random proportions that sum exactly to 1.

## Usage

``` r
rSum21(size, digits = NULL)
```

## Arguments

- size:

  integer. The number of values to generate.

- digits:

  integer. If not `NULL` (default), the values are rounded to this
  number of decimal places while preserving the sum of 1.

## Value

a numeric vector of length `size` summing to 1.

## Details

The values are drawn from a uniform distribution and normalized by their
sum. If `digits` is given, the values are rounded and the rounding error
is assigned to the largest element, which is then rounded again to the
requested precision. Note that for very coarse rounding the exact-sum
guarantee may not be attainable at the given precision.

## Examples

``` r
x <- rSum21(5)
sum(x)
#> [1] 1

x <- rSum21(5, digits = 2)
sum(x)
#> [1] 1
```
