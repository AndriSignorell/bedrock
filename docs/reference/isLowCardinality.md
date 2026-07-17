# Check for Low Cardinality

Checks whether `x` contains at most `maxUnique` unique, non-missing
values. Unlike [`nUnique()`](nUnique.md), this stops counting as soon as
the threshold is exceeded, which makes it considerably faster for large,
high-cardinality vectors.

## Usage

``` r
isLowCardinality(x, maxUnique = 12)
```

## Arguments

- x:

  a numeric or integer vector.

- maxUnique:

  integer. The threshold up to which `x` is considered to have low
  cardinality. Defaults to `12`.

## Value

a logical of length one: `TRUE` if `x` has `maxUnique` or fewer unique,
non-`NA` values, `FALSE` otherwise.

## See also

[`nUnique()`](nUnique.md) for the uncapped count.

Other data.predicate: [`flags()`](flags.md),
[`isDichotomous()`](isDichotomous.md), [`isEuclid()`](isEuclid.md),
[`isNumeric()`](isNumeric.md), [`isWholeLike()`](isWholeLike.md),
[`isZero()`](isZero.md), [`nUnique()`](nUnique.md)

## Examples

``` r
isLowCardinality(c(1, 2, 2, 3, NA))
#> [1] TRUE

isLowCardinality(1:100, maxUnique = 12)
#> [1] FALSE
```
