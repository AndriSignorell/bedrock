# Test Whether Multiple Objects Are Identical

Extends [`identical`](https://rdrr.io/r/base/identical.html) to more
than two objects. Returns `TRUE` if all supplied objects are exactly
identical, and `FALSE` otherwise.

## Usage

``` r
allIdentical(...)
```

## Arguments

- ...:

  Objects to compare.

## Value

Logical scalar.

## See also

[`identical`](https://rdrr.io/r/base/identical.html)

Other data.inspection: [`allDuplicated()`](allDuplicated.md),
[`completeColumns()`](completeColumns.md),
[`countCompCases()`](countCompCases.md), [`flags()`](flags.md),
[`isDichotomous()`](isDichotomous.md), [`isNumeric()`](isNumeric.md),
[`isWholeLike()`](isWholeLike.md), [`isZero()`](isZero.md)

## Examples

``` r
A <- LETTERS[1:5]
B <- LETTERS[1:5]
C <- LETTERS[1:5]
E <- factor(LETTERS[1:5])

allIdentical(A, B, C)        # TRUE
#> [1] TRUE
allIdentical(A, B, C, E)     # FALSE
#> [1] FALSE
```
