# Logit Transformation and Its Inverse

Computes the logit transformation and its inverse for values defined on
a finite interval \\\[min, max\]\\.

## Usage

``` r
logit(x, min = 0, max = 1, eps = .Machine$double.eps, warn = FALSE)

logitInv(x, min = 0, max = 1)
```

## Arguments

- x:

  numeric vector. For `logit()`, values are interpreted relative to the
  interval \\\[min, max\]\\. For `logitInv()`, `x` can be any real
  number.

- min:

  lower bound of the interval. Must be finite.

- max:

  upper bound of the interval. Must be finite and greater than `min`.

- eps:

  small positive value used to clamp probabilities away from \\0\\ and
  \\1\\ for numerical stability in `logit()`. Default:
  `.Machine$double.eps`.

- warn:

  logical; if `TRUE`, a warning is issued when values are effectively
  clamped because they fall outside \\(eps, 1 - eps)\\ after rescaling.
  Default: `FALSE`.

## Value

A numeric vector of the same length as `x`.

## Details

The `logit()` function maps values from \\\[min, max\]\\ to the real
line \\(-\infty, \infty)\\. The inverse transformation `logitInv()` maps
real-valued inputs back to \\\[min, max\]\\.

The logit transformation is defined as:

\$\$ \mathrm{logit}(x) = \log\left(\frac{p}{1 - p}\right) \$\$

where

\$\$ p = \frac{x - min}{max - min}. \$\$

For numerical stability, \\p\\ is clamped to \\\[eps, 1 - eps\]\\ before
applying the transformation. This prevents returning `-Inf` or `Inf` for
values exactly equal to `min` or `max`, or slightly outside the interval
due to floating point error.

If `warn = TRUE`, a warning is issued when such clamping occurs.

The inverse transformation is given by:

\$\$ x = min + (max - min) \cdot \frac{1}{1 + e^{-z}} \$\$

where \\z\\ is the input to `logitInv()`.

Note that `logitInv()` does not perform clamping. This asymmetry is
intentional: [`plogis`](https://rdrr.io/r/stats/Logistic.html) is
well-defined for all real inputs, so no stabilization is required.

## See also

[`qlogis`](https://rdrr.io/r/stats/Logistic.html),
[`plogis`](https://rdrr.io/r/stats/Logistic.html)

Other math.transform: [`linScale()`](linScale.md),
[`percentRank()`](percentRank.md), [`rankX()`](rankX.md),
[`winsorize()`](winsorize.md)

## Examples

``` r
x <- seq(0, 1, length.out = 5)
z <- logit(x)
logitInv(z)
#> [1] 2.220446e-16 2.500000e-01 5.000000e-01 7.500000e-01 1.000000e+00

# Boundary values are clamped internally:
# 0 -> eps, 1 -> 1 - eps
logit(c(0, 0.5, 1))
#> [1] -36.04365   0.00000  36.04365

# With warn = TRUE, clamping at the boundaries triggers a warning
logit(c(0, 0.5, 1), warn = TRUE)
#> Warning: Values outside (min, max) were clamped to avoid -Inf/Inf
#> [1] -36.04365   0.00000  36.04365

# Values strictly outside the interval also trigger a warning
logit(c(-0.1, 0.5, 1.1), warn = TRUE)
#> Warning: Values outside (min, max) were clamped to avoid -Inf/Inf
#> [1] -36.04365   0.00000  36.04365

# Custom interval
x <- seq(10, 20, length.out = 5)
z <- logit(x, min = 10, max = 20)
logitInv(z, min = 10, max = 20)
#> [1] 10.0 12.5 15.0 17.5 20.0
```
