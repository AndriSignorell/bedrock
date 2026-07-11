# Count Unique Values

Returns the number of unique elements in a vector.

## Usage

``` r
nUnique(x, na.rm = FALSE)
```

## Arguments

- x:

  a vector

- na.rm:

  logical. Should missing values (`NA`) be removed before counting
  unique values? Defaults to `FALSE`.

## Value

An integer of length one.

## See also

[`base::nlevels()`](https://rdrr.io/r/base/nlevels.html),
[`isLowCardinality()`](isLowCardinality.md) to check whether `x` has at
most a given number of unique values, without counting all of them
first.

Other data.predicate: [`flags()`](flags.md),
[`isDichotomous()`](isDichotomous.md), [`isEuclid()`](isEuclid.md),
[`isLowCardinality()`](isLowCardinality.md),
[`isNumeric()`](isNumeric.md), [`isWholeLike()`](isWholeLike.md),
[`isZero()`](isZero.md)

## Examples

``` r
nUnique(c(1, 1, 2, 3))
#> [1] 3

nUnique(c(1, 1, 2, NA))
#> [1] 3

nUnique(c(1, 1, 2, NA), na.rm = TRUE)
#> [1] 2
```
