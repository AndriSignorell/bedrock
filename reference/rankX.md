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

  one or more vectors to be ranked. If multiple vectors are provided,
  they are ranked lexicographically (like `order`). All inputs must have
  the same length.

- decreasing:

  logical; if `TRUE`, larger values receive smaller ranks (i.e., ranking
  in descending order). When ranking multiple inputs, a logical vector
  may be given to control the direction per input.

- na.last:

  logical or `"keep"`; determines the placement of `NA` values. Passed
  to
  [`data.table::frankv`](https://rdrr.io/pkg/data.table/man/frank.html).

- ties.method:

  character string specifying how ties are handled. One of:

  `"average"`

  :   average of the ranks for tied values (default).

  `"first"`

  :   ranks assigned in order of appearance.

  `"last"`

  :   ranks assigned in reverse order of appearance.

  `"random"`

  :   ranks assigned at random.

  `"max"`

  :   maximum rank for tied values.

  `"min"`

  :   minimum rank for tied values.

  `"dense"`

  :   like `"min"`, but ranks are consecutive integers without gaps.

## Value

an integer or numeric vector of ranks with the same length as the input.

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

Other math.transform: [`linScale()`](linScale.md),
[`logit()`](logit.md), [`percentRank()`](percentRank.md),
[`winsorize()`](winsorize.md)

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
