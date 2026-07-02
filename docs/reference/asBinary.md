# Coerce a Vector to Binary (0/1)

A unified conversion utility for binary variables. Handles the four most
common input types and provides consistent behavior across all of them.

For **logical** input, `TRUE` is mapped to 1 and `FALSE` to 0.

For **numeric** input, values must already be 0 or 1 (or `NA`); any
other value raises an error.

For **factor** input, the vector must have exactly two levels. By
default the second level (alphabetically) is coded as 1. Use `ref` to
specify which level should be coded as 1.

For **character** input, the vector must have exactly two distinct
non-missing values. The same `ref` logic applies.

## Usage

``` r
asBinary(x, ref = NULL, warn = TRUE)
```

## Arguments

- x:

  a logical, numeric, integer, factor, or character vector.

- ref:

  optional reference value. If supplied, observations equal to `ref` are
  coded as 1 and all others as 0. Must be one of the observed values or
  factor levels.

- warn:

  logical. If `TRUE` (default), a warning is issued when a factor or
  character vector is coerced to binary without an explicit `ref`,
  indicating which value is coded as 1.

## Value

a numeric vector of 0s and 1s (and `NA`s where present in `x`).

## Details

Converts a logical, numeric, factor, or character vector to a binary
numeric vector coded as 0 and 1.

## See also

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md),
[`asCDateFmt()`](asCDateFmt.md), [`columnWrap()`](columnWrap.md),
[`combLevels()`](combLevels.md),
[`compareDataFrames()`](compareDataFrames.md), [`dummy()`](dummy.md),
[`nf()`](nf.md), [`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`renameX()`](renameX.md), [`revCode()`](revCode.md),
[`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`splitAt()`](splitAt.md), [`splitX()`](splitX.md),
[`stringsAsFactors()`](stringsAsFactors.md), [`toBaseR()`](toBaseR.md)

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

# factor with explicit reference
asBinary(factor(c("control", "treatment", "control")), ref = "treatment")
#> [1] 0 1 0

# character
asBinary(c("no", "yes", "no", "yes"))
#> Warning: coercing character to binary (0/1): using 'yes' as '1'
#> [1] 0 1 0 1
#> attr(,"coding")
#>  no yes 
#>   0   1 

# character with explicit reference
asBinary(c("F", "U", "F", "U"), ref = "F")
#> [1] 1 0 1 0
```
