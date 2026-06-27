# Interval overlap length

Computes the length of the overlap between intervals.

Convenience operator for interval overlap.

## Usage

``` r
overlap(x, y)

x %overlaps% y
```

## Arguments

- x:

  Numeric vector of length 2 or matrix with 2 columns

- y:

  Numeric vector of length 2 or matrix with 2 columns

- x, y:

  Numeric intervals

## Value

Numeric vector of overlap lengths (0 if no overlap)

Logical vector

## Details

Intervals are treated as closed intervals \[a, b\]. Therefore, intervals
that only share a boundary point have overlap length 0.

## Examples

``` r
overlap(c(1, 5), c(3, 7))  # 2
#> [1] 2
overlap(c(1, 3), c(3, 5))  # 0
#> [1] 0

c(1,5) %overlaps% c(3,7)
#> [1] TRUE
```
