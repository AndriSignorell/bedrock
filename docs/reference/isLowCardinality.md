# Check for Low Cardinality

Checks whether `x` contains at most `maxUnique` unique, non-missing
values. Unlike
[`nUnique()`](https://andrisignorell.github.io/bedrock/reference/nUnique.md),
this stops counting as soon as the threshold is exceeded, which makes it
considerably faster for large, high-cardinality vectors.

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

[`nUnique()`](https://andrisignorell.github.io/bedrock/reference/nUnique.md)
for the uncapped count.

Other data.predicate:
[`flags()`](https://andrisignorell.github.io/bedrock/reference/flags.md),
[`isDichotomous()`](https://andrisignorell.github.io/bedrock/reference/isDichotomous.md),
[`isEuclid()`](https://andrisignorell.github.io/bedrock/reference/isEuclid.md),
[`isNumeric()`](https://andrisignorell.github.io/bedrock/reference/isNumeric.md),
[`isWholeLike()`](https://andrisignorell.github.io/bedrock/reference/isWholeLike.md),
[`isZero()`](https://andrisignorell.github.io/bedrock/reference/isZero.md),
[`nUnique()`](https://andrisignorell.github.io/bedrock/reference/nUnique.md)

## Examples

``` r
isLowCardinality(c(1, 2, 2, 3, NA))
#> [1] TRUE

isLowCardinality(1:100, maxUnique = 12)
#> [1] FALSE
```
