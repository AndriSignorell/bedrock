# Fast Ranking with Extended Tie Handling

Computes ranks for vectors or multiple inputs using a fast
implementation based on
[`data.table::frankv`](https://rdrr.io/pkg/data.table/man/frank.html).
Supports additional tie-handling methods such as `"dense"` and
multi-column ranking via `...`.

## Usage

``` r
rankX(
  ...,
  decreasing = FALSE,
  na.last = TRUE,
  ties.method = c("average", "first", "last", "random", "max", "min", "dense")
)
```

## Arguments

- ...:

  One or more vectors to be ranked. If multiple vectors are provided,
  they are ranked lexicographically (like `order`). All inputs must have
  the same length.

- decreasing:

  Logical; if `TRUE`, larger values receive smaller ranks (i.e., ranking
  in descending order).

- na.last:

  Logical or `"keep"`; determines the placement of `NA` values. Passed
  to
  [`data.table::frankv`](https://rdrr.io/pkg/data.table/man/frank.html).

- ties.method:

  Character string specifying how ties are handled. One of:

  `"average"`

  :   Average of the ranks for tied values (default).

  `"first"`

  :   Ranks assigned in order of appearance.

  `"last"`

  :   Ranks assigned in reverse order of appearance.

  `"random"`

  :   Ranks assigned at random.

  `"max"`

  :   Maximum rank for tied values.

  `"min"`

  :   Minimum rank for tied values.

  `"dense"`

  :   Like `"min"`, but ranks are consecutive integers without gaps.

## Value

An integer or numeric vector of ranks with the same length as the input.

## Details

This function is a fast alternative to
[`rank`](https://rdrr.io/r/base/rank.html), powered by
[`data.table::frankv`](https://rdrr.io/pkg/data.table/man/frank.html).
It extends base functionality by:

- Supporting dense ranking (`ties.method = "dense"`)

- Allowing multiple input vectors for lexicographic ranking

- Providing improved performance for large datasets

When multiple inputs are supplied, ranking is performed jointly, similar
to:


    order(x1, x2, ...)

## See also

[`rank`](https://rdrr.io/r/base/rank.html),
[`frankv`](https://rdrr.io/pkg/data.table/man/frank.html)

Other math.utils: [`crossProd()`](crossProd.md),
[`crossProdN()`](crossProdN.md), [`dotProd()`](dotProd.md),
[`linScale()`](linScale.md), [`logit()`](logit.md),
[`nUnique()`](nUnique.md), [`percentRank()`](percentRank.md),
[`precision`](precision.md), [`roundTo()`](roundTo.md),
[`unirootAll()`](unirootAll.md), [`untable()`](untable.md)

## Examples

``` r
x <- c(10, 20, 20, 30)

# Basic ranking
rankX(x)
#> [1] 1.0 2.5 2.5 4.0

# Dense ranking
rankX(x, ties.method = "dense")
#> [1] 1 2 2 3

# Descending order
rankX(x, decreasing = TRUE)
#> [1] 4.0 2.5 2.5 1.0

# Handling NA values
x2 <- c(3, NA, 1, 2)
rankX(x2, na.last = "keep")
#> [1]  3 NA  1  2

# Multi-column ranking
a <- c(1, 1, 2, 2)
b <- c(2, 1, 2, 1)
rankX(a, b)
#> [1] 2 1 4 3
```
