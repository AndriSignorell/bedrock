# Round to a Multiple

Rounds the values of a numeric vector to the nearest multiple of a given
step width. Where [`round()`](https://rdrr.io/r/base/Round.html) is tied
to multiples of a power of ten, `roundTo()` accepts an arbitrary step,
so that prices can be rounded to the nearest 5 cents, durations to the
nearest quarter of an hour or axis limits to the nearest 250. The
direction of the rounding is controlled by `FUN`, which allows rounding
to the nearest, upwards, downwards or towards zero with the same
interface.

## Usage

``` r
roundTo(x, multiple = 1, FUN = round)
```

## Arguments

- x:

  numeric. The values to be rounded.

- multiple:

  numeric. The step width to whose multiples the values are to be
  rounded, defaults to `1`. Must be finite and positive and either a
  single value or as long as `x`.

- FUN:

  the rounding function applied to `x / multiple`. Typically one of
  [`round()`](https://rdrr.io/r/base/Round.html) (default),
  [`trunc()`](https://rdrr.io/r/base/Round.html),
  [`ceiling()`](https://rdrr.io/r/base/Round.html) or
  [`floor()`](https://rdrr.io/r/base/Round.html). Other functions
  accepting and returning a numeric vector can be used as well.

## Value

a numeric vector of the rounded values, as long as `x`. `NA`s in `x` are
returned as `NA`.

## Details

There are several functions in base R to convert to integers.
[`round()`](https://rdrr.io/r/base/Round.html) rounds to the nearest
integer or to any number of digits. Using a negative number of digits
rounds to a power of ten, so that `round(x, -3)` rounds to thousands.
Each of [`trunc()`](https://rdrr.io/r/base/Round.html),
[`floor()`](https://rdrr.io/r/base/Round.html) and
[`ceiling()`](https://rdrr.io/r/base/Round.html) rounds in a fixed
direction, towards zero, down and up respectively.
[`round()`](https://rdrr.io/r/base/Round.html) is documented to round
half to even, so `round(2.5)` is `2`.

`roundTo()` evaluates `FUN(x / multiple) * multiple`. With the default
`FUN = round` a value lying exactly halfway between two multiples is
therefore rounded to the one with the even quotient: `roundTo(1, 2)` is
`0` and `roundTo(3, 2)` is `4`. Setting `FUN = ceiling` always rounds
up, `FUN = floor` always rounds down and `FUN = trunc` always towards
zero (see the examples for a comparison).

Ties are rare in practice, as most decimal fractions have no exact
binary representation. `1.3 / 0.2` is marginally smaller than `6.5` in
double precision, so `roundTo(1.3, 0.2)` returns `1.2` and not the `1.4`
that the rule for ties would suggest. Results for a fractional
`multiple` are likewise only accurate to within representation error,
which is why `roundTo(x, 0.05)` may still print more than two decimal
places.

A single `multiple` is used for all the values in `x`. A vector of step
widths is applied elementwise and must then be exactly as long as `x`,
so that a length mismatch is reported as an error instead of being
recycled silently.

## See also

[`round()`](https://rdrr.io/r/base/Round.html),
[`trunc()`](https://rdrr.io/r/base/Round.html),
[`ceiling()`](https://rdrr.io/r/base/Round.html),
[`floor()`](https://rdrr.io/r/base/Round.html)

Other math.basic: [`closest()`](closest.md),
[`crossProd()`](crossProd.md), [`crossProdN()`](crossProdN.md),
[`dotProd()`](dotProd.md), [`unirootAll()`](unirootAll.md)

## Examples

``` r

roundTo(10, 3)     # rounds 10 to the nearest multiple of 3 (9)
#> [1] 9
roundTo(-10, 3)    # rounds -10 to the nearest multiple of 3 (-9)
#> [1] -9

roundTo(1.3, 0.2)  # rounds 1.3 to the nearest multiple of 0.2 (1.2)
#> [1] 1.2
roundTo(-1.3, 0.2) # rounds -1.3 to the nearest multiple of 0.2 (-1.2)
#> [1] -1.2

# prices to the nearest 5 cents
roundTo(c(1.02, 1.03, 12.375), 0.05)
#> [1]  1.00  1.05 12.40

# a step width for every value
roundTo(c(1.23, 123, 1234), c(0.05, 10, 100))
#> [1]    1.25  120.00 1200.00

# any other length is an error, the values are not recycled
try(roundTo(1:6, c(2, 3)))
#> Error : length of 'multiple' [2] must be 1 or the length of 'x' [6]

# round down
roundTo(c(1, -1) * 1.2335, 0.05, floor)
#> [1]  1.20 -1.25
roundTo(c(1, -1) * 1233.5, 100, floor)
#> [1]  1200 -1300

# round up
roundTo(c(1, -1) * 1.2335, 0.05, ceiling)
#> [1]  1.25 -1.20
roundTo(c(1, -1) * 1233.5, 100, ceiling)
#> [1]  1300 -1200

# round towards zero
roundTo(c(1, -1) * 1.2335, 0.05, trunc)
#> [1]  1.2 -1.2
roundTo(c(1, -1) * 1233.5, 100, trunc)
#> [1]  1200 -1200

# the four directions side by side
x <- c(-1.5, -1.3, 1.3, 1.5)
cbind(x       = x,
      round   = roundTo(x, 0.2, FUN = round),
      trunc   = roundTo(x, 0.2, FUN = trunc),
      ceiling = roundTo(x, 0.2, FUN = ceiling),
      floor   = roundTo(x, 0.2, FUN = floor)
)
#>         x round trunc ceiling floor
#> [1,] -1.5  -1.6  -1.4    -1.4  -1.6
#> [2,] -1.3  -1.2  -1.2    -1.2  -1.4
#> [3,]  1.3   1.2   1.2     1.4   1.2
#> [4,]  1.5   1.6   1.4     1.6   1.4

# note how the ties in the first column are resolved to even multiples
x <- -5:5
cbind(x       = x,
      round   = roundTo(x, 2, FUN = round),
      trunc   = roundTo(x, 2, FUN = trunc),
      ceiling = roundTo(x, 2, FUN = ceiling),
      floor   = roundTo(x, 2, FUN = floor)
)
#>        x round trunc ceiling floor
#>  [1,] -5    -4    -4      -4    -6
#>  [2,] -4    -4    -4      -4    -4
#>  [3,] -3    -4    -2      -2    -4
#>  [4,] -2    -2    -2      -2    -2
#>  [5,] -1     0     0       0    -2
#>  [6,]  0     0     0       0     0
#>  [7,]  1     0     0       2     0
#>  [8,]  2     2     2       2     2
#>  [9,]  3     4     2       4     2
#> [10,]  4     4     4       4     4
#> [11,]  5     4     4       6     4

```
