# Coerce a Vector to Binary (0/1)

A unified conversion utility for binary variables. Converts a logical,
numeric, factor, or character vector to a binary numeric vector coded as
0 and 1.

## Usage

``` r
asBinary(x, pos = NULL, warn = TRUE)
```

## Arguments

- x:

  a logical, numeric, integer, factor, or character vector.

- pos:

  optional positive value. If supplied, observations equal to `pos` are
  coded as 1 and all others as 0. Must be one of the observed values or
  factor levels.

- warn:

  logical. If `TRUE` (default), a warning is issued when a factor or
  character vector is coerced to binary without an explicit `pos`,
  indicating which value is coded as 1.

## Value

a numeric vector of 0s and 1s (and `NA`s where present in `x`). For
factor and character input, the result carries a `"coding"` attribute, a
named integer vector documenting which original value was mapped to 0
and which to 1.

## Details

For **logical** input, `TRUE` is mapped to 1 and `FALSE` to 0.

For **numeric** input, values must already be 0 or 1 (or `NA`); any
other value raises an error.

For **factor** input, the vector must have exactly two levels. By
default the second level is coded as 1. Use `pos` to specify which level
should be coded as 1.

For **character** input, the vector must have exactly two distinct
non-missing values. By default the alphabetically second value is coded
as 1. The same `pos` logic applies.

## See also

Other data.recode: [`combLevels()`](combLevels.md),
[`dummy()`](dummy.md), [`mReplace()`](mReplace.md), [`nf()`](nf.md),
[`recodeX()`](recodeX.md), [`revCode()`](revCode.md),
[`stringsAsFactors()`](stringsAsFactors.md)

## Examples

``` r
# logical
asBinary(c(TRUE, FALSE, TRUE))
#> [1] 1 0 1

# numeric (already binary)
asBinary(c(0, 1, 1, 0))
#> [1] 0 1 1 0

# factor: second level coded as 1 by default
asBinary(factor(c("control", "treatment", "control")))
#> Warning: coercing factor to binary (0/1): using 'treatment' as '1'
#> [1] 0 1 0
#> attr(,"coding")
#>   control treatment 
#>         0         1 

# factor with explicit positive value
asBinary(factor(c("control", "treatment", "control")), pos = "treatment")
#> [1] 0 1 0
#> attr(,"coding")
#>   control treatment 
#>         0         1 

# character
asBinary(c("no", "yes", "no", "yes"))
#> Warning: coercing character to binary (0/1): using 'yes' as '1'
#> [1] 0 1 0 1
#> attr(,"coding")
#>  no yes 
#>   0   1 

# character with explicit positive value
asBinary(c("F", "U", "F", "U"), pos = "F")
#> [1] 1 0 1 0
#> attr(,"coding")
#> U F 
#> 0 1 
```
