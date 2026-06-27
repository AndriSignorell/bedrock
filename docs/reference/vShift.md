# Shift a vector with NA padding

Shifts a vector to the left or right by `k` positions. Vacated positions
are filled with `NA`.

## Usage

``` r
vShift(x, k = 1L)
```

## Arguments

- x:

  A vector.

- k:

  Integer. Number of positions to shift. Positive values shift to the
  right, negative values to the left.

## Value

A vector of the same length as `x`, shifted with `NA` padding.

## Details

Unlike `VecRot()`, this function does not wrap elements around. Elements
shifted beyond the vector bounds are discarded.

## See also

Other vector.ops: [`closest()`](closest.md),
[`coalesceX()`](coalesceX.md), [`midx()`](midx.md),
[`moveAvg()`](moveAvg.md), [`naIf()`](naIf.md),
[`naReplace()`](naReplace.md), [`nz()`](nz.md),
[`pairApply()`](pairApply.md), [`quot()`](quot.md),
[`rankX()`](rankX.md), [`splitAt()`](splitAt.md), [`trim()`](trim.md),
[`unwhich()`](unwhich.md), [`vRot()`](vRot.md),
[`winsorize()`](winsorize.md)

## Examples

``` r
vShift(1:5, 2)
#> [1] NA NA  1  2  3
# NA NA 1 2 3

vShift(1:5, -2)
#> [1]  3  4  5 NA NA
# 3 4 5 NA NA

vShift(1:5, 10)
#> [1] NA NA NA NA NA
# NA NA NA NA NA
```
