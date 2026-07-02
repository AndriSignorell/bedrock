# Number of Combinations of a Set

Return the number of combinations with and without replacement and
order.

## Usage

``` r
combN(n, m, repl = FALSE, ord = FALSE)
```

## Arguments

- n:

  number of elements from which to choose.

- m:

  number of elements to choose. For `combSet` can `m` be a numeric
  vector too.

- repl:

  logical. Should repetition of the same element be allowed? Defaults to
  `FALSE`

- ord:

  logical. Does the order matter? Default is `FALSE`.

## Value

an integer value

## See also

[`combPairs`](combPairs.md),
[`combn`](https://rdrr.io/r/utils/combn.html),
[`choose`](https://rdrr.io/r/base/Special.html),
[`factorial`](https://rdrr.io/r/base/Special.html),  
`vignette("Combinatorics")`

Other combinatorics: [`combPairs()`](combPairs.md),
[`combSet()`](combinatoric.md), [`permn()`](permn.md),
[`randGroupSplit()`](randGroupSplit.md), [`sampleX()`](sampleX.md),
[`unwhich()`](unwhich.md)

## Examples

``` r
n <- 5; m <- 2
combN(n, m, repl=TRUE, ord=FALSE)
#> [1] 15
combN(n, m, repl=TRUE, ord=TRUE)
#> [1] 25
combN(n, m, repl=FALSE, ord=TRUE)
#> [1] 20
combN(n, m, repl=FALSE, ord=FALSE)
#> [1] 10
```
