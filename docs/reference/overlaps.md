# Check if intervals overlap

Returns TRUE if intervals overlap, FALSE otherwise.

## Usage

``` r
overlaps(x, y)
```

## Arguments

- x:

  Numeric vector of length 2 or matrix with 2 columns

- y:

  Numeric vector of length 2 or matrix with 2 columns

## Value

Logical vector

## Details

Intervals are treated as closed intervals \[a, b\], meaning that
intervals sharing a boundary point are considered overlapping.

## Examples

``` r
overlaps(c(1, 5), c(3, 7))  # TRUE
#> [1] TRUE
overlaps(c(1, 3), c(3, 5))  # TRUE
#> [1] TRUE
overlaps(c(1, 2), c(3, 4))  # FALSE
#> [1] FALSE
```
