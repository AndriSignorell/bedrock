# Lagged Quotients

Returns suitably lagged and iterated quotients.

## Usage

``` r
quot(x, lag = 1L, quotients = 1L, ...)
```

## Arguments

- x:

  a numeric vector or matrix containing the values to be used for
  calculating the quotients.

- lag:

  an integer indicating which lag to use.

- quotients:

  an integer indicating the order of the quotient.

- ...:

  further arguments to be passed to or from methods.

## Value

if `x` is a vector of length `n` and `quotients = 1`, then the computed
result is equal to the successive quotients
`x[(1+lag):n] / x[1:(n-lag)]`.

If `quotients` is larger than one this algorithm is applied recursively
to `x`. Note that the returned value is a vector which is shorter than
`x`.

If `x` is a matrix then the division operations are carried out on each
column separately.

## Details

[`NA`](https://rdrr.io/r/base/NA.html)'s propagate.

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

## See also

[`diff`](https://rdrr.io/r/base/diff.html)

Other vector.window: [`midx()`](midx.md), [`moveAvg()`](moveAvg.md)

## Examples

``` r

quot(1:10, 2)
#> [1] 3.000000 2.000000 1.666667 1.500000 1.400000 1.333333 1.285714 1.250000
quot(1:10, 2, 2)
#> [1] 0.5555556 0.7500000 0.8400000 0.8888889 0.9183673 0.9375000
x <- cumprod(cumprod(1:10))
quot(x, lag = 2)
#> [1] 1.200000e+01 1.440000e+02 2.880000e+03 8.640000e+04 3.628800e+06
#> [6] 2.032128e+08 1.463132e+10 1.316819e+12
quot(x, quotients = 2)
#> [1]  3  4  5  6  7  8  9 10
```
