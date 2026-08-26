# Count Unique Values

Returns the number of unique elements in a vector.

## Usage

``` r
nUnique(x, na.rm = FALSE)
```

## Arguments

- x:

  a vector.

- na.rm:

  logical. Should missing values (`NA`) be removed before counting
  unique values? Defaults to `FALSE`.

## Value

an integer of length one.

## See also

[`base::nlevels()`](https://rdrr.io/r/base/nlevels.html),
[`isLowCardinality()`](https://andrisignorell.github.io/bedrock/reference/isLowCardinality.md)
to check whether `x` has at most a given number of unique values,
without counting all of them first.

Other data.predicate:
[`flags()`](https://andrisignorell.github.io/bedrock/reference/flags.md),
[`isDichotomous()`](https://andrisignorell.github.io/bedrock/reference/isDichotomous.md),
[`isEuclid()`](https://andrisignorell.github.io/bedrock/reference/isEuclid.md),
[`isLowCardinality()`](https://andrisignorell.github.io/bedrock/reference/isLowCardinality.md),
[`isNumeric()`](https://andrisignorell.github.io/bedrock/reference/isNumeric.md),
[`isWholeLike()`](https://andrisignorell.github.io/bedrock/reference/isWholeLike.md),
[`isZero()`](https://andrisignorell.github.io/bedrock/reference/isZero.md)

## Examples

``` r
nUnique(c(1, 1, 2, 3))
#> [1] 3

nUnique(c(1, 1, 2, NA))
#> [1] 3

nUnique(c(1, 1, 2, NA), na.rm = TRUE)
#> [1] 2
```
