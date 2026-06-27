# Trim a Vector

Clean data by means of trimming, i.e., by omitting outlying
observations.

## Usage

``` r
trim(x, trim = 0.1, na.rm = FALSE)
```

## Arguments

- x:

  a numeric vector to be trimmed.

- trim:

  the fraction (0 to 0.5) of observations to be trimmed from each end
  of x. Values of trim outside that range (and \< 1) are taken as the
  nearest endpoint. If `trim` is set to a value \>1 it's interpreted as
  the number of elements to be cut off at each tail of `x`.

- na.rm:

  a logical value indicating whether `NA` values should be stripped
  before the computation proceeds.

## Value

The trimmed vector `x`. The indices of the trimmed values will be
attached as attribute named `"trim"`.

## Details

A symmetrically trimmed vector `x` with a fraction of trim observations
(resp. the given number) deleted from each end will be returned. If
`trim` is set to a value \>0.5 or to an integer value \> n/2 then the
result will be `NA`.

## Note

This function is basically an excerpt from the base function
[`mean`](https://rdrr.io/r/base/mean.html), which allows the vector `x`
to be trimmed before calculating the mean. But what if a trimmed
standard deviation is needed?

## See also

Other vector.ops: [`closest()`](closest.md),
[`coalesceX()`](coalesceX.md), [`midx()`](midx.md),
[`moveAvg()`](moveAvg.md), [`naIf()`](naIf.md),
[`naReplace()`](naReplace.md), [`nz()`](nz.md),
[`pairApply()`](pairApply.md), [`quot()`](quot.md),
[`rankX()`](rankX.md), [`splitAt()`](splitAt.md),
[`unwhich()`](unwhich.md), [`vRot()`](vRot.md), [`vShift()`](vShift.md),
[`winsorize()`](winsorize.md)

## Examples

``` r

## generate data
set.seed(1234)     # for reproducibility
x <- rnorm(10)     # standard normal
x[1] <- x[1] * 10  # introduce outlier

## Trim data
x
#>  [1] -12.0706575   0.2774292   1.0844412  -2.3456977   0.4291247   0.5060559
#>  [7]  -0.5747400  -0.5466319  -0.5644520  -0.8900378
trim(x, trim=0.1)
#> [1]  0.2774292 -2.3456977  0.4291247  0.5060559 -0.5747400 -0.5466319 -0.5644520
#> [8] -0.8900378
#> attr(,"trim")
#> [1] 1 3

## Trim fixed number, say cut the 3 extreme elements from each end
trim(x, trim=3)
#> [1]  0.2774292 -0.5747400 -0.5466319 -0.5644520
#> attr(,"trim")
#> [1]  1  4 10  5  6  3

## check function
s <- sample(10:20)
s.tr <- trim(s, trim = 2)
setequal(c(s[attr(s.tr, "trim")], s.tr), s)
#> [1] TRUE
```
