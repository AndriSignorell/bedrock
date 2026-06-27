# Count Complete Cases

Return for each variable of a data frame the number of missing values
and the complete cases to be expected if this variable would be omitted.

## Usage

``` r
countCompCases(x)
```

## Arguments

- x:

  a data.frame containg the data.

## Value

A list with three elements. The first gives the number of rows, the
second the number of complete cases for the whole data frame. The third
element `tab` contains the data for the single variables.

## Details

Count Complete Cases

Return for each variable of a data frame the number of missing values
and the complete cases to be expected if this variable would be omitted.

## See also

`plotMiss`,
[`complete.cases`](https://rdrr.io/r/stats/complete.cases.html),
[`is.na`](https://rdrr.io/r/base/NA.html),
[`na.omit`](https://rdrr.io/r/stats/na.fail.html)

Other data.inspection: [`allDuplicated()`](allDuplicated.md),
[`allIdentical()`](allIdentical.md),
[`completeColumns()`](completeColumns.md), [`flags()`](flags.md),
[`isDichotomous()`](isDichotomous.md), [`isNumeric()`](isNumeric.md),
[`isWholeLike()`](isWholeLike.md), [`isZero()`](isZero.md)

## Examples

``` r
countCompCases(airquality)
#> $n
#> [1] 153
#> 
#> $cc
#> [1] 111
#> 
#> $tab
#>     vname nas      nas_p cifnot  cifnot_p
#> 1   Ozone  37 0.24183007    146 0.9542484
#> 2 Solar.R   7 0.04575163    116 0.7581699
#> 3    Wind   0 0.00000000    111 0.7254902
#> 4    Temp   0 0.00000000    111 0.7254902
#> 5   Month   0 0.00000000    111 0.7254902
#> 6     Day   0 0.00000000    111 0.7254902
#> 
#> attr(,"class")
#> [1] "CountCompCases"

```
