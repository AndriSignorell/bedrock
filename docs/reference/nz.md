# Extract Non-Zero Values

Returns all non-zero elements of a vector.

## Usage

``` r
nz(x)
```

## Arguments

- x:

  A numeric vector.

## Value

A vector containing only the non-zero elements of `x`.

## Details

This is a convenience function primarily intended for use in model
formulas or quick vector filtering.

## See also

Other vector.ops: [`closest()`](closest.md),
[`coalesceX()`](coalesceX.md), [`midx()`](midx.md),
[`moveAvg()`](moveAvg.md), [`naIf()`](naIf.md),
[`naReplace()`](naReplace.md), [`pairApply()`](pairApply.md),
[`quot()`](quot.md), [`rankX()`](rankX.md), [`splitAt()`](splitAt.md),
[`trim()`](trim.md), [`unwhich()`](unwhich.md), [`vRot()`](vRot.md),
[`vShift()`](vShift.md), [`winsorize()`](winsorize.md)
