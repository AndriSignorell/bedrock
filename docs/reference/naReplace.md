# Replace NA Values

Replaces `NA` values in a vector or factor with a specified value.

## Usage

``` r
naReplace(x, value)

# Default S3 method
naReplace(x, value)

# S3 method for class 'factor'
naReplace(x, value)
```

## Arguments

- x:

  a vector or factor

- value:

  the replacement value. For factors, a single character string.

## Value

An object of the same class as `x` with `NA` values replaced by `value`.

## Details

For factors (including ordered factors), `value` is appended as a new
level at the last position if it is not already present. If `value` is
an existing level, the missing values are simply filled with it.

## See also

Other vector.na: [`coalesceX()`](coalesceX.md), [`isNA()`](isNA.md),
[`locf()`](locf.md), [`naIf()`](naIf.md)

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

# ordered factor: the new level is appended at the end
naReplace(factor(c("low", "high", NA), levels = c("low", "high"),
                 ordered = TRUE), "unknown")
#> [1] low     high    unknown
#> Levels: low < high < unknown
```
