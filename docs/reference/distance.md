# Interval distance

Computes the distance between non-overlapping intervals. Returns 0 if
intervals overlap (including boundary touching).

## Usage

``` r
distance(x, y)
```

## Arguments

- x:

  Numeric vector of length 2 or matrix with 2 columns

- y:

  Numeric vector of length 2 or matrix with 2 columns

## Value

Numeric vector of distances (0 if overlapping)

## Examples

``` r
distance(c(1, 2), c(4, 5))  # 2
#> [1] 2
distance(c(1, 5), c(3, 7))  # 0
#> [1] 0
distance(c(1, 3), c(3, 5))  # 0
#> [1] 0
```
