# Check Whether a Vector Is Dichotomous

Determines whether a vector contains at most two distinct values.

## Usage

``` r
isDichotomous(x, strict = FALSE, na.rm = FALSE)
```

## Arguments

- x:

  a vector.

- strict:

  logical. If `TRUE`, exactly two distinct values must be present. If
  `FALSE` (default), at most two distinct values are allowed.

- na.rm:

  logical. If `TRUE`, missing values are removed before evaluation. If
  `FALSE` (default), the presence of `NA` results in `NA` (indeterminate
  status).

## Value

`TRUE`, `FALSE`, or `NA` if the status cannot be determined because of
missing values (see `na.rm`).

## See also

Other data.predicate:
[`flags()`](https://andrisignorell.github.io/bedrock/reference/flags.md),
[`isEuclid()`](https://andrisignorell.github.io/bedrock/reference/isEuclid.md),
[`isLowCardinality()`](https://andrisignorell.github.io/bedrock/reference/isLowCardinality.md),
[`isNumeric()`](https://andrisignorell.github.io/bedrock/reference/isNumeric.md),
[`isWholeLike()`](https://andrisignorell.github.io/bedrock/reference/isWholeLike.md),
[`isZero()`](https://andrisignorell.github.io/bedrock/reference/isZero.md),
[`nUnique()`](https://andrisignorell.github.io/bedrock/reference/nUnique.md)

## Examples

``` r
isDichotomous(c(0, 1, 1))
#> [1] TRUE
isDichotomous(c(1, 1, 1))
#> [1] TRUE
isDichotomous(c(1, 1, 1), strict = TRUE)
#> [1] FALSE
isDichotomous(c(0, 1, NA))               # NA
#> [1] NA
isDichotomous(c(0, 1, NA), na.rm = TRUE)
#> [1] TRUE
isDichotomous(c("A", "A", "B"))
#> [1] TRUE
isDichotomous(c("A", "A", "B", "C"))
#> [1] FALSE
isDichotomous(factor(c("A", "A", "B", "C")))
#> [1] FALSE
```
