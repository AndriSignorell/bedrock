# Rename Elements of a Named Object

Renames selected elements of a named object by specifying old-to-new
name mappings. Works on any R object that supports
[`names()`](https://rdrr.io/r/base/names.html), including vectors,
lists, data frames, and matrices. For matrix-like objects, `rownames`
and `colnames` can be targeted via the `which` argument.

## Usage

``` r
renameX(x, ..., on = "names", useGsub = FALSE, fixed = TRUE, warn = TRUE)
```

## Arguments

- x:

  a named object. Any type that supports
  [`names()`](https://rdrr.io/r/base/names.html),
  [`rownames()`](https://rdrr.io/r/base/colnames.html), or
  [`colnames()`](https://rdrr.io/r/base/colnames.html), e.g. a vector,
  list, data frame, or matrix.

- ...:

  name mappings of the form `old = "new"`, a single function to apply to
  all names (e.g. `toupper`), or unnamed character strings applied
  positionally (see Details).

- on:

  character scalar specifying which names to operate on. One of
  `"names"` (default), `"rownames"`, or `"colnames"`. Partial matching
  is supported.

- useGsub:

  logical scalar. If `TRUE`, each mapping is applied as a
  [`gsub()`](https://rdrr.io/r/base/grep.html) pattern substitution
  across all current names rather than an exact replacement. Default is
  `FALSE`.

- fixed:

  logical scalar. Passed to [`gsub()`](https://rdrr.io/r/base/grep.html)
  when `useGsub = TRUE`. If `TRUE` (default), patterns are treated as
  fixed strings rather than regular expressions.

- warn:

  logical scalar. If `TRUE` (default), a warning is issued when one or
  more old names supplied in `...` are not found in the targeted names
  of `x`. Only relevant in exact mode (`useGsub = FALSE`).

## Value

the object `x` with updated names; all other attributes are preserved.

## Details

The function supports three modes:

- Exact mode (`useGsub = FALSE`, default):

  Names are matched exactly via
  [`match()`](https://rdrr.io/r/base/match.html). Each element of `...`
  must be a named scalar character string of the form `old = "new"`.
  Unmatched old names trigger a warning when `warn = TRUE`.

- Pattern mode (`useGsub = TRUE`):

  Each mapping is treated as a
  [`gsub()`](https://rdrr.io/r/base/grep.html) substitution applied in
  sequence to *all* current names. The left-hand side is the pattern,
  the right-hand side is the replacement. The `fixed` argument is
  forwarded to [`gsub()`](https://rdrr.io/r/base/grep.html).

- Function mode:

  If a single function is passed in `...`, it is applied to all current
  names. Useful for bulk transformations such as `toupper`, `tolower`,
  or `make.names`.

When `...` contains unnamed character elements, the names are assigned
positionally: the first element replaces `names(x)[1]`, the second
`names(x)[2]`, and so on.

## See also

[`names`](https://rdrr.io/r/base/names.html),
[`setNames`](https://rdrr.io/r/stats/setNames.html)

Other label.attrs: [`label()`](label.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md)

## Examples

``` r
x <- c(a = 1, b = 2, c = 3)

# Exact mode: rename by old = "new" pairs
renameX(x, a = "alpha", c = "gamma")
#> alpha     b gamma 
#>     1     2     3 

# Positional mode: replaces names(x)[1:2]
renameX(x, "alpha", "beta")
#> alpha  beta     c 
#>     1     2     3 

# Function mode: apply a function to all names
renameX(x, toupper)
#> A B C 
#> 1 2 3 
renameX(x, tolower)
#> a b c 
#> 1 2 3 

# Data frame columns
d <- data.frame(foo = 1:3, bar = 4:6)
renameX(d, foo = "x", bar = "y")
#>   x y
#> 1 1 4
#> 2 2 5
#> 3 3 6

# Pattern mode: strip a common prefix
y <- c(v_mean = 1, v_sd = 2, v_n = 3)
renameX(y, v_ = "", useGsub = TRUE)
#> mean   sd    n 
#>    1    2    3 

# Pattern mode with regex (fixed = FALSE)
renameX(y, `^v_` = "", useGsub = TRUE, fixed = FALSE)
#> mean   sd    n 
#>    1    2    3 

# Matrix: rename colnames selectively
m <- matrix(1:6, nrow = 2,
            dimnames = list(c("row_a", "row_b"), c("col_x", "col_y", "col_z")))
renameX(m, col_x = "alpha", on = "colnames")
#>       alpha col_y col_z
#> row_a     1     3     5
#> row_b     2     4     6

# Matrix: uppercase all rownames via function mode
renameX(m, toupper, on = "rownames")
#>       col_x col_y col_z
#> ROW_A     1     3     5
#> ROW_B     2     4     6

# Matrix: rename rownames via gsub
renameX(m, `row_` = "", useGsub = TRUE, fixed = FALSE, on = "rownames")
#>   col_x col_y col_z
#> a     1     3     5
#> b     2     4     6
```
