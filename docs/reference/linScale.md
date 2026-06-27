# Linearly Rescale Numeric Data

Performs a linear transformation of numeric data to a specified range.
Each column of `x` is rescaled independently.

## Usage

``` r
linScale(x, low = NULL, high = NULL, newLow = 0, newHigh = 1)
```

## Arguments

- x:

  A numeric vector, matrix or data frame.

- low, high:

  Optional numeric vectors specifying the lower and upper bounds of the
  original scale. If `NULL`, the column-wise minima and maxima of `x`
  are used.

- newLow, newHigh:

  Numeric vectors specifying the target range. Defaults to `0` and `1`.

## Value

A numeric matrix with the same dimensions as `x`, where each column is
linearly rescaled to the interval `[newLow, newHigh]`.

## Details

The transformation is defined as: \$\$ x\_{scaled} = \frac{x -
low}{high - low} \cdot (new\\high - new\\low) + new\\low \$\$

Constant columns (where `high == low`) are mapped to `newLow`.

## See also

[`scale`](https://rdrr.io/r/base/scale.html)

## Examples

``` r
x <- matrix(1:10, ncol = 2)

# default scaling to [0,1]
linScale(x)
#>      [,1] [,2]
#> [1,] 0.00 0.00
#> [2,] 0.25 0.25
#> [3,] 0.50 0.50
#> [4,] 0.75 0.75
#> [5,] 1.00 1.00

# custom range
linScale(x, newLow = -1, newHigh = 1)
#>      [,1] [,2]
#> [1,] -1.0 -1.0
#> [2,] -0.5 -0.5
#> [3,]  0.0  0.0
#> [4,]  0.5  0.5
#> [5,]  1.0  1.0

# using predefined bounds
linScale(x, low = 1, high = 10)
#>           [,1]      [,2]
#> [1,] 0.0000000 0.5555556
#> [2,] 0.1111111 0.6666667
#> [3,] 0.2222222 0.7777778
#> [4,] 0.3333333 0.8888889
#> [5,] 0.4444444 1.0000000
```
