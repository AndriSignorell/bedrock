# Replace NA Values

Replaces `NA` values in a vector or factor with a specified value.

## Usage

``` r
naReplace(x, value)

# Default S3 method
naReplace(x, value)

# S3 method for class 'factor'
naReplace(x, value)

# S3 method for class 'ordered'
naReplace(x, value)
```

## Arguments

- x:

  A vector or factor.

- value:

  The replacement value. For factors, a character string that will be
  added as a new level if not already present.

## Value

An object of the same class as `x` with `NA` values replaced by `value`.

## See also

Other vector.ops: [`closest()`](closest.md),
[`coalesceX()`](coalesceX.md), [`locf()`](locf.md), [`midx()`](midx.md),
[`moveAvg()`](moveAvg.md), [`naIf()`](naIf.md), [`nz()`](nz.md),
[`pairApply()`](pairApply.md), [`setLength()`](setLength.md),
[`trim()`](trim.md), [`vRot()`](vRot.md), [`vShift()`](vShift.md),
[`winsorize()`](winsorize.md)

## Examples

``` r
# default: numeric vector
naReplace(c(1, NA, 3), 0)
#> [1] 1 0 3

# character vector
naReplace(c("a", NA, "c"), "missing")
#> [1] "a"       "missing" "c"      

# unordered factor
naReplace(factor(c("a", "b", NA)), "missing")
#> [1] a       b       missing
#> Levels: a b missing

# ordered factor
naReplace(factor(c("low", "high", NA), levels = c("low", "high"),
                 ordered = TRUE), "medium")
#> [1] low  high <NA>
#> Levels: low < high < medium
```
