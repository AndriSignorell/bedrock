# Winsorize a Numeric Vector

Winsorization replaces extreme values in a numeric vector by less
extreme, predefined bounds. Values below a lower limit are set to that
limit, and values above an upper limit are set to that upper limit.

## Usage

``` r
winsorize(x, val = quantile(x, probs = c(0.05, 0.95), na.rm = TRUE))
```

## Arguments

- x:

  A numeric vector to be winsorized.

- val:

  A numeric vector of length two specifying the lower and upper
  winsorization limits. Defaults to the 5% and 95% quantiles of `x` with
  `na.rm = TRUE`.

## Value

A numeric vector of the same length as `x`, where:

- values below the lower limit are replaced by the lower limit

- values above the upper limit are replaced by the upper limit

- missing values remain unchanged

## Details

By default, the limits are defined as the 5% and 95% quantiles of the
data. Missing values are ignored when computing quantiles and are
preserved in the output.

Formally, the winsorized vector \\g(x)\\ is defined as: \$\$ g(x) =
\left\\ \begin{array}{ll} l & \text{if } x \le l \\ x & \text{if } l \<
x \< u \\ u & \text{if } x \ge u \end{array} \right. \$\$ where \\l\\
and \\u\\ denote the lower and upper bounds.

The argument `val` allows full control over the limits. It can be:

- A numeric vector of length two specifying fixed bounds

- The result of a call to
  [`quantile`](https://rdrr.io/r/stats/quantile.html) (e.g. with custom
  `type`)

Winsorization is commonly used in robust statistics to reduce the
influence of outliers. In some cases, it can be beneficial to
standardize the data (e.g., using
[`scale`](https://rdrr.io/r/base/scale.html)) before applying
winsorization.

## See also

`scaleX`, [`winsorize`](https://rdrr.io/pkg/robustHD/man/winsorize.html)

Other vector.ops: [`closest()`](closest.md),
[`coalesceX()`](coalesceX.md), [`midx()`](midx.md),
[`moveAvg()`](moveAvg.md), [`naIf()`](naIf.md),
[`naReplace()`](naReplace.md), [`nz()`](nz.md),
[`pairApply()`](pairApply.md), [`quot()`](quot.md),
[`rankX()`](rankX.md), [`splitAt()`](splitAt.md), [`trim()`](trim.md),
[`unwhich()`](unwhich.md), [`vRot()`](vRot.md), [`vShift()`](vShift.md)

## Examples

``` r
set.seed(9128)
x <- c(rnorm(10), NA, -100, 100)

# Default winsorization (5% / 95% quantiles)
winsorize(x)
#>  [1]  -1.09088409  -0.02449403   0.12549029   0.75983517   0.77969152
#>  [6]   0.04945601   0.74742272  -0.17505077   0.58035461  -0.59841061
#> [11]           NA -45.59998625  45.42883034

# Winsorization using fixed bounds
winsorize(x, val = c(-10, 10))
#>  [1]  -1.09088409  -0.02449403   0.12549029   0.75983517   0.77969152
#>  [6]   0.04945601   0.74742272  -0.17505077   0.58035461  -0.59841061
#> [11]           NA -10.00000000  10.00000000

# Custom quantile definition
winsorize(x, val = quantile(x, c(0.1, 0.9), type = 1, na.rm = TRUE))
#>  [1] -1.09088409 -0.02449403  0.12549029  0.75983517  0.77969152  0.04945601
#>  [7]  0.74742272 -0.17505077  0.58035461 -0.59841061          NA -1.09088409
#> [13]  0.77969152

# One-sided winsorization
winsorize(x, val = c(-Inf, 2))  # upper bound only
#>  [1]   -1.09088409   -0.02449403    0.12549029    0.75983517    0.77969152
#>  [6]    0.04945601    0.74742272   -0.17505077    0.58035461   -0.59841061
#> [11]            NA -100.00000000    2.00000000
winsorize(x, val = c(-2, Inf)) # lower bound only
#>  [1]  -1.09088409  -0.02449403   0.12549029   0.75983517   0.77969152
#>  [6]   0.04945601   0.74742272  -0.17505077   0.58035461  -0.59841061
#> [11]           NA  -2.00000000 100.00000000
```
