# Split a Character Vector into a Dummy Matrix

Splits a character vector of delimited tokens into a binary dummy
data.frame where each unique token becomes a column.

## Usage

``` r
strSplitToDummy(x, split = ",", trim = TRUE, na.action = na.pass, ...)
```

## Arguments

- x:

  a character vector with delimited tokens.

- split:

  a character string to use as delimiter. Default is `","`.

- trim:

  logical. If `TRUE` (default), whitespace is trimmed from each token
  after splitting.

- na.action:

  a function to handle `NA` values. Accepted values are
  [`na.pass`](https://rdrr.io/r/stats/na.fail.html) (default),
  [`na.omit`](https://rdrr.io/r/stats/na.fail.html),
  [`na.exclude`](https://rdrr.io/r/stats/na.fail.html), and
  [`na.fail`](https://rdrr.io/r/stats/na.fail.html).

  `na.pass`

  :   NAs are kept as all-zero rows (default).

  `na.omit`

  :   rows with NAs are silently removed.

  `na.exclude`

  :   like `na.omit` but the indices of removed rows are stored in a
      `"na.action"` attribute.

  `na.fail`

  :   an error is raised if any `NA` is present.

- ...:

  additional arguments passed to
  [`strsplit`](https://rdrr.io/r/base/strsplit.html).

## Value

a `data.frame` with one row per element of `x` and one column per unique
token. Values are `0L` or `1L`. Column names are the token values as-is
and may not be syntactically valid R identifiers. The attribute
`"tokens"` contains the sorted vector of unique tokens.

## See also

[`strsplit`](https://rdrr.io/r/base/strsplit.html),
[`na.omit`](https://rdrr.io/r/stats/na.fail.html)

Other string.transform: [`mGsub()`](mGsub.md),
[`strSplitToCol()`](strSplitToCol.md)

## Examples

``` r
dat <- data.frame(id = 1:5,
                  txt = c("A,C,D", "A", "B,C", "D", "D,E"))

# default: NA passed through as zero row
strSplitToDummy(dat$txt)
#>   A B C D E
#> 1 1 0 1 1 0
#> 2 1 0 0 0 0
#> 3 0 1 1 0 0
#> 4 0 0 0 1 0
#> 5 0 0 0 1 1

# with an NA in the input
x_na <- c("A,B", "B,C", NA, "A")

# na.pass: NA becomes an all-zero row (default)
strSplitToDummy(x_na, na.action = na.pass)
#>   A B C
#> 1 1 1 0
#> 2 0 1 1
#> 3 0 0 0
#> 4 1 0 0

# na.omit: NA rows are silently dropped
strSplitToDummy(x_na, na.action = na.omit)
#>   A B C
#> 1 1 1 0
#> 2 0 1 1
#> 3 1 0 0

# na.exclude: like na.omit but NA indices stored in attribute
res <- strSplitToDummy(x_na, na.action = na.exclude)
attr(res, "na.action")
#> [1] 3
#> attr(,"class")
#> [1] "exclude"

# na.fail: error if any NA present
tryCatch(
  strSplitToDummy(x_na, na.action = na.fail),
  error = function(e) conditionMessage(e)
)
#> [1] "missing values in 'x'"
```
