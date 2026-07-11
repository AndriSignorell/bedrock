# Rotate a vector

Rotates a vector cyclically to the right by `k` positions. Negative
values of `k` rotate to the left.

## Usage

``` r
vRot(x, k = 1L)
```

## Arguments

- x:

  a vector

- k:

  integer. Number of positions to rotate (default = 1).

## Value

A vector of the same length as `x`, rotated cyclically.

## Details

The rotation is cyclic, meaning elements shifted off one end reappear on
the other.

## See also

Other vector.reshape: [`setLength()`](setLength.md),
[`trim()`](trim.md), [`vShift()`](vShift.md)

## Examples

``` r
vRot(1:5, 2)
#> [1] 4 5 1 2 3
# 4 5 1 2 3

vRot(1:5, -1)
#> [1] 2 3 4 5 1
# 2 3 4 5 1
```
