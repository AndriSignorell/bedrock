# Number of Combinations of a Set

Return the number of combinations with and without replacement and
order.

## Usage

``` r
combN(n, m, replace = FALSE, ordered = FALSE)
```

## Arguments

- n:

  number of elements from which to choose.

- m:

  number of elements to choose. For `combSet` can `m` be a numeric
  vector too.

- replace:

  logical; whether repetition of the same element is allowed. Defaults
  to `FALSE`.

- ordered:

  logical. Does the order matter? Default is `FALSE`.

## Value

a numeric value.

## See also

[`combn`](https://rdrr.io/r/utils/combn.html),
[`choose`](https://rdrr.io/r/base/Special.html),
[`factorial`](https://rdrr.io/r/base/Special.html),
`vignette("Combinatorics", package = "bedrock")`

Other combinatorics: [`combPairs()`](combPairs.md),
[`combSet()`](combSet.md), [`pairApply()`](pairApply.md),
[`permn()`](permn.md), [`randGroupSplit()`](randGroupSplit.md),
[`sampleX()`](sampleX.md)

## Examples

``` r
n <- 5; m <- 2
combN(n, m, replace=TRUE, ordered=FALSE)
#> [1] 15
combN(n, m, replace=TRUE, ordered=TRUE)
#> [1] 25
combN(n, m, replace=FALSE, ordered=TRUE)
#> [1] 20
combN(n, m, replace=FALSE, ordered=FALSE)
#> [1] 10
```
