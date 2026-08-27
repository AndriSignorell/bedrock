# Round to Multiple

Returns a number rounded to the nearest specified multiple.

## Usage

``` r
roundTo(x, multiple = 1, FUN = round)
```

## Arguments

- x:

  numeric. The value to round.

- multiple:

  numeric. The multiple to which the number is to be rounded. Default is
  1.

- FUN:

  the rounding function as character or as expression. Can be one out of
  [`trunc`](https://rdrr.io/r/base/Round.html), `ceiling`, `round`
  (default) or `floor`.

## Value

the rounded value.

## Details

There are several functions to convert to integers.
[`round`](https://rdrr.io/r/base/Round.html) rounds to the nearest
integer or to any number of digits. Using a negative number rounds to a
power of ten, so that `round (x, -3)` rounds to thousands. Each of
[`trunc`](https://rdrr.io/r/base/Round.html),
[`floor`](https://rdrr.io/r/base/Round.html) and
[`ceiling`](https://rdrr.io/r/base/Round.html) round in a fixed
direction, towards zero, down and up respectively.
[`round`](https://rdrr.io/r/base/Round.html) is documented to round to
even, so `round(2.5)` is `2`.

`roundTo` uses `round(x/multiple)*multiple` to get the result. So if `x`
is equally close to two multiples, the multiple with the smaller
absolute value will be returned when `round(x/multiple)` is even (and
the greater when it's odd).  
If `FUN` is set to `ceiling` it will always round up, and if set to
`floor` it will always round down. See examples for comparison).

## See also

[`round`](https://rdrr.io/r/base/Round.html),
[`trunc`](https://rdrr.io/r/base/Round.html),
[`ceiling`](https://rdrr.io/r/base/Round.html),
[`floor`](https://rdrr.io/r/base/Round.html)

Other math.basic: [`closest()`](closest.md),
[`crossProd()`](crossProd.md), [`crossProdN()`](crossProdN.md),
[`dotProd()`](dotProd.md), [`unirootAll()`](unirootAll.md)

## Examples

``` r

roundTo(10, 3)     # Rounds 10 to a nearest multiple of 3 (9)
#> [1] 9
roundTo(-10, -3)   # Rounds -10 to a nearest multiple of -3 (-9)
#> [1] -9

roundTo(1.3, 0.2)  # Rounds 1.3 to a nearest multiple of 0.2 (1.2)
#> [1] 1.2
roundTo(-1.3, 0.2) # Rounds -1.3 to a nearest multiple of 0.2 (-1.2)
#> [1] -1.2
# Different signs of x and multiple raise an error
try(roundTo(5, -2))
#> Error in roundTo(5, -2) : 
#>   a negative `multiple` cannot be combined with positive `x`.

# Round down
roundTo(c(1,-1) * 1.2335, 0.05, floor)
#> [1]  1.20 -1.25
roundTo(c(1,-1) * 1233.5, 100, floor)
#> [1]  1200 -1300

# Round up
roundTo(c(1,-1) * 1.2335, 0.05, ceiling)
#> [1]  1.25 -1.20
roundTo(c(1,-1) * 1233.5, 100, ceiling)
#> [1]  1300 -1200

# Round towards zero
roundTo(c(1,-1) * 1.2335, 0.05, trunc)
#> [1]  1.2 -1.2
roundTo(c(1,-1) * 1233.5, 100, trunc)
#> [1]  1200 -1200


x <- c(-1.5,-1.3, 1.3, 1.5)
cbind(x =       x,
      round =   roundTo(x, 0.2, FUN=round),
      trunc =   roundTo(x, 0.2, FUN=trunc),
      ceiling = roundTo(x, 0.2, FUN=ceiling),
      floor =   roundTo(x, 0.2, FUN=floor)
)
#>         x round trunc ceiling floor
#> [1,] -1.5  -1.6  -1.4    -1.4  -1.6
#> [2,] -1.3  -1.2  -1.2    -1.2  -1.4
#> [3,]  1.3   1.2   1.2     1.4   1.2
#> [4,]  1.5   1.6   1.4     1.6   1.4

x <- -10:10
cbind(x =       x,
      round =   roundTo(x, 2, FUN=round),
      trunc =   roundTo(x, 2, FUN=trunc),
      ceiling = roundTo(x, 2, FUN=ceiling),
      floor =   roundTo(x, 2, FUN=floor)
)
#>         x round trunc ceiling floor
#>  [1,] -10   -10   -10     -10   -10
#>  [2,]  -9    -8    -8      -8   -10
#>  [3,]  -8    -8    -8      -8    -8
#>  [4,]  -7    -8    -6      -6    -8
#>  [5,]  -6    -6    -6      -6    -6
#>  [6,]  -5    -4    -4      -4    -6
#>  [7,]  -4    -4    -4      -4    -4
#>  [8,]  -3    -4    -2      -2    -4
#>  [9,]  -2    -2    -2      -2    -2
#> [10,]  -1     0     0       0    -2
#> [11,]   0     0     0       0     0
#> [12,]   1     0     0       2     0
#> [13,]   2     2     2       2     2
#> [14,]   3     4     2       4     2
#> [15,]   4     4     4       4     4
#> [16,]   5     4     4       6     4
#> [17,]   6     6     6       6     6
#> [18,]   7     8     6       8     6
#> [19,]   8     8     8       8     8
#> [20,]   9     8     8      10     8
#> [21,]  10    10    10      10    10

```
