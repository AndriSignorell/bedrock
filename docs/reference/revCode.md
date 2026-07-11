# Reverse Coding of Variables

Reverses the coding of a vector. Supports numeric, logical, and factor
inputs:

- **Numeric**: Transforms values using `min + max - x`

- **Logical**: Flips TRUE/FALSE

- **Factor**: Reverses the order of levels

## Usage

``` r
revCode(x, min = NULL, max = NULL, na.rm = FALSE)
```

## Arguments

- x:

  a vector (numeric, logical, or factor)

- min:

  optional numeric minimum. Must be provided together with `max`. If
  `NULL` (default), the observed minimum of `x` is used.

- max:

  optional numeric maximum. Must be provided together with `min`. If
  `NULL` (default), the observed maximum of `x` is used.

- na.rm:

  logical; whether to ignore `NA`s when computing the range (numeric
  only). If `FALSE` and `NA`s are present, a warning is issued and `NA`
  is returned for all values. Default is `FALSE`.

## Value

A vector of the same type and length as `x`, with reversed coding.

## Errors

Throws an error if all values are `NA`, if only one of `min`/`max` is
provided, if `min > max`, or if `x` is not numeric, logical, or factor.
A warning is issued if values of `x` lie outside an explicitly provided
`[min, max]` range.

## See also

Other data.recode: [`asBinary()`](asBinary.md),
[`combLevels()`](combLevels.md), [`dummy()`](dummy.md),
[`mReplace()`](mReplace.md), [`nf()`](nf.md), [`recodeX()`](recodeX.md),
[`stringsAsFactors()`](stringsAsFactors.md)

## Examples

``` r
# Numeric
revCode(c(1, 2, 3, 4, 5))
#> [1] 5 4 3 2 1

# Numeric with explicit range (e.g., Likert scale)
revCode(c(1, 2, 3, 4, 5), min = 1, max = 5)
#> [1] 5 4 3 2 1

# Numeric with NAs
revCode(c(1, 2, NA, 4, 5), na.rm = TRUE)
#> [1]  5  4 NA  2  1

# Logical
revCode(c(TRUE, FALSE, TRUE))
#> [1] FALSE  TRUE FALSE

# Factor
x <- factor(c("low", "medium", "high"), ordered = TRUE)
revCode(x)
#> [1] low    high   medium
#> Levels: medium < low < high
```
