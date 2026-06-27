# Percent Rank of a Numeric Vector

Computes the percent rank of each element in a numeric vector. The
percent rank is defined as: \$\$(rank(x) - 1) / (n - 1)\$\$ where \\n\\
is the number of non-missing observations.

## Usage

``` r
percentRank(x)
```

## Arguments

- x:

  A numeric (or comparable) vector.

## Value

A numeric vector of the same length as `x`, containing values between 0
and 1.

## Details

This corresponds to the definition used in SQL and
[`dplyr::percent_rank()`](https://dplyr.tidyverse.org/reference/percent_rank.html).

The smallest value in `x` receives a percent rank of 0, and the largest
value receives a percent rank of 1 (if there are at least two
non-missing values).

Ties are handled using `ties.method = "min"` via [`rankX`](rankX.md),
meaning tied values receive the same minimal rank.

Missing values (`NA`) are preserved in the output.

If `x` contains fewer than two non-missing values, all results are `NA`.

## See also

[`rankX`](rankX.md), [`rank`](https://rdrr.io/r/base/rank.html)

Other math.utils: [`crossProd()`](crossProd.md),
[`crossProdN()`](crossProdN.md), [`divisors()`](divisors.md),
[`dotProd()`](dotProd.md), [`gcd_lcm`](gcd_lcm.md),
[`precision`](precision.md), [`ptInPoly()`](ptInPoly.md),
[`roundTo()`](roundTo.md), [`unirootAll()`](unirootAll.md)

## Examples

``` r
x <- c(10, 20, 20, 30)
percentRank(x)
#> [1] 0.0000000 0.3333333 0.3333333 1.0000000

# With ties
x <- c(1, 2, 2, 3)
percentRank(x)
#> [1] 0.0000000 0.3333333 0.3333333 1.0000000

# With missing values
x <- c(3, NA, 1, 2)
percentRank(x)
#> [1] 1.0  NA 0.0 0.5

# Single non-missing value
percentRank(c(5, NA, NA))
#> [1] NA NA NA
```
