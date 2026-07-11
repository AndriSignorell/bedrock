# Last Observation Carried Forward

In longitudinal studies it's common that individuals drop out before all
responses can be obtained. Measurements obtained before the individual
dropped out can be used to impute the unknown measurement(s). The last
observation carried forward method is one way to impute values for the
missing observations. For the last observation carried forward (LOCF)
approach the missing values are replaced by the last observed value of
that variable for each individual regardless of when it occurred.

## Usage

``` r
locf(x)
```

## Arguments

- x:

  a vector, a data.frame or a matrix containing NAs

## Value

an object of the same type and dimension as `x`.

## Details

`locf()` replaces `NA`s with the most recent non-NA prior to it.

The function will replace all NAs found in a vector with the last
earlier value not being NA. In data frames and matrices each column is
treated separately, so that values are never carried across column
boundaries. Factors are supported and keep their levels and ordering.

It should be noted, that the last observation carried forward approach
may result in biased estimates and may underestimate the variability.

## Note

Based on code by Daniel Wollschlaeger, adapted to conform to package
standards; multi-column, data-frame, and factor support added by the
package author.

## See also

See also the package Hmisc for less coarse imputation functions.

Other vector.na: [`coalesceX()`](coalesceX.md), [`isNA()`](isNA.md),
[`naIf()`](naIf.md), [`naReplace()`](naReplace.md)

## Examples

``` r

d.frm <- data.frame(
  day=rep(c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), 4)
, val=rep(c(runif(5), rep(NA,2)), 4) )

d.frm$locf <- locf( d.frm$val )
d.frm
#>    day        val       locf
#> 1  mon 0.64855298 0.64855298
#> 2  tue 0.06010516 0.06010516
#> 3  wed 0.05253167 0.05253167
#> 4  thu 0.21957116 0.21957116
#> 5  fri 0.13193263 0.13193263
#> 6  sat         NA 0.13193263
#> 7  sun         NA 0.13193263
#> 8  mon 0.64855298 0.64855298
#> 9  tue 0.06010516 0.06010516
#> 10 wed 0.05253167 0.05253167
#> 11 thu 0.21957116 0.21957116
#> 12 fri 0.13193263 0.13193263
#> 13 sat         NA 0.13193263
#> 14 sun         NA 0.13193263
#> 15 mon 0.64855298 0.64855298
#> 16 tue 0.06010516 0.06010516
#> 17 wed 0.05253167 0.05253167
#> 18 thu 0.21957116 0.21957116
#> 19 fri 0.13193263 0.13193263
#> 20 sat         NA 0.13193263
#> 21 sun         NA 0.13193263
#> 22 mon 0.64855298 0.64855298
#> 23 tue 0.06010516 0.06010516
#> 24 wed 0.05253167 0.05253167
#> 25 thu 0.21957116 0.21957116
#> 26 fri 0.13193263 0.13193263
#> 27 sat         NA 0.13193263
#> 28 sun         NA 0.13193263
```
