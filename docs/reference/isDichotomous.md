# Check Whether a Vector Is Dichotomous

Determines whether a vector contains at most two distinct values.

## Usage

``` r
isDichotomous(x, strict = FALSE, na.rm = FALSE)
```

## Arguments

- x:

  A vector.

- strict:

  Logical. If `TRUE`, exactly two distinct values must be present. If
  `FALSE` (default), at most two distinct values are allowed.

- na.rm:

  Logical. If `TRUE`, missing values are removed before evaluation. If
  `FALSE` (default), the presence of `NA` results in `FALSE`.

## Value

A single logical value.

## See also

Other data.inspection: [`allDuplicated()`](allDuplicated.md),
[`allIdentical()`](allIdentical.md),
[`completeColumns()`](completeColumns.md),
[`countCompCases()`](countCompCases.md), [`flags()`](flags.md),
[`isNumeric()`](isNumeric.md), [`isWholeLike()`](isWholeLike.md),
[`isZero()`](isZero.md)

## Examples

``` r
isDichotomous(c(0, 1, 1))
#> [1] TRUE
isDichotomous(c(1, 1, 1))
#> [1] TRUE
isDichotomous(c(1, 1, 1), strict = TRUE)
#> [1] FALSE
isDichotomous(c(0, 1, NA), na.rm = TRUE)
#> [1] TRUE
isDichotomous(c("A", "A", "B"))
#> [1] TRUE
isDichotomous(c("A", "A", "B", "C"))
#> [1] FALSE
isDichotomous(factor(c("A", "A", "B", "C")))
#> [1] FALSE
```
