# Check a Vector For Being Zero

Test if x is zero. This is done by checking if the numeric value is
below the machine tolerance.

## Usage

``` r
isZero(x, tol = sqrt(.Machine$double.eps), na.rm = FALSE)
```

## Arguments

- x:

  a (non-empty) numeric or complex vector of data values.

- tol:

  tolerance to be used.

- na.rm:

  logical, indicating whether `NA` values should be stripped before the
  computation proceeds. Defaults to `FALSE`.

## Value

logical vector of the same length as x (after optional `NA` removal).
Non-numeric input yields all-`FALSE`.

## See also

[`all.equal`](https://rdrr.io/r/base/all.equal.html),
[`isWholeLike`](https://andrisignorell.github.io/bedrock/reference/isWholeLike.md)

Other data.predicate:
[`flags()`](https://andrisignorell.github.io/bedrock/reference/flags.md),
[`isDichotomous()`](https://andrisignorell.github.io/bedrock/reference/isDichotomous.md),
[`isEuclid()`](https://andrisignorell.github.io/bedrock/reference/isEuclid.md),
[`isLowCardinality()`](https://andrisignorell.github.io/bedrock/reference/isLowCardinality.md),
[`isNumeric()`](https://andrisignorell.github.io/bedrock/reference/isNumeric.md),
[`isWholeLike()`](https://andrisignorell.github.io/bedrock/reference/isWholeLike.md),
[`nUnique()`](https://andrisignorell.github.io/bedrock/reference/nUnique.md)

## Examples

``` r
# "... These are people who live in ignorance of the Floating Point Gods.
# These pagans expect [...] the following to be TRUE" (Burns, 2011):
(.1 - .3 / 3) == 0
#> [1] FALSE

# they might be helped by
isZero(.1 - .3 / 3)
#> [1] TRUE
```
