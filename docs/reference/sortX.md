# Sort Vectors, Matrices, Tables, and Data Frames

`sortX` extends the base [`sort`](https://rdrr.io/r/base/sort.html)
function by providing a consistent interface for sorting not only
vectors, but also matrices, tables, and data frames. For 2-dimensional
objects, rows are sorted based on one or more columns.

## Usage

``` r
sortX(x, ...)

# Default S3 method
sortX(
  x,
  decreasing = FALSE,
  na.last = NA,
  method = c("default", "mixed"),
  factorsAsCharacter = TRUE,
  ...
)

# S3 method for class 'table'
sortX(
  x,
  ord = NULL,
  decreasing = FALSE,
  na.last = TRUE,
  method = c("default", "mixed"),
  factorsAsCharacter = TRUE,
  ...
)

# S3 method for class 'matrix'
sortX(
  x,
  ord = NULL,
  decreasing = FALSE,
  na.last = TRUE,
  method = c("default", "mixed"),
  factorsAsCharacter = TRUE,
  ...
)

# S3 method for class 'data.frame'
sortX(
  x,
  ord = NULL,
  decreasing = FALSE,
  na.last = TRUE,
  method = c("default", "mixed"),
  factorsAsCharacter = TRUE,
  ...
)
```

## Arguments

- x:

  a numeric, complex, character or logical vector, factor, matrix,
  table, or data frame to be sorted

- ...:

  further arguments passed to [`sort`](https://rdrr.io/r/base/sort.html)
  in `sortX.default`

- decreasing:

  logical scalar or vector. Should the sort be in decreasing order? For
  2-dimensional objects a vector of the same length as `ord` may be
  supplied to control the direction per column; a scalar is recycled.

- na.last:

  logical or `NA`. Should missing values be placed last (`TRUE`), first
  (`FALSE`), or removed (`NA`)? See
  [`order`](https://rdrr.io/r/base/order.html).

- method:

  sorting method. Either `"default"` (base R behavior) or `"mixed"` for
  natural sorting of character data (e.g. `"A2"` \< `"A10"`).

- factorsAsCharacter:

  logical. If `TRUE` (default), factors are converted to character
  before sorting so that labels are used instead of level codes. Set to
  `FALSE` to sort by level order (useful for ordered factors).

- ord:

  integer or character vector specifying the columns to sort by, and
  their priority (first element = primary key). Column names and
  positive integer indices (`1:ncol(x)`) refer to columns. The special
  value `0L` (integer zero, always numeric) sorts by row names. For
  `table` and `matrix` objects, `ncol(x) + 1L` sorts by row marginal
  sums. This argument is not available for `sortX.default`. Default:
  `NULL` (all columns, left to right).

## Value

The sorted object, of the same class as `x`.

## Details

By default, sorting follows the behavior of base R. In addition,
`method = "mixed"` enables natural ("human-friendly") sorting of
character data, e.g. `"A2"` \< `"A10"`.

For `method = "mixed"`, sorting is applied column-wise using
`.orderMixed()`. Each column's tokens are ordered independently (numeric
runs numerically, text runs lexicographically) before the results are
combined via stable right-to-left ordering.

The sort order for factors depends on `factorsAsCharacter`: if `TRUE`
(default), factors are sorted by their labels (alphabetically or by
natural order when `method = "mixed"`); if `FALSE`, they are sorted by
their level order, which is appropriate for ordered factors but may be
unintuitive for unordered ones.

## See also

[`sort`](https://rdrr.io/r/base/sort.html),
[`order`](https://rdrr.io/r/base/order.html)

Other data.order: [`binaryTree()`](binaryTree.md), [`revX()`](revX.md)

## Examples

``` r
set.seed(3)
d.frm <- iris[sample(nrow(iris), 10),
              c("Species", "Sepal.Length", "Sepal.Width")]

# Vector sorting
sortX(d.frm[, 1])
#>  [1] setosa     setosa     setosa     setosa     versicolor virginica 
#>  [7] virginica  virginica  virginica  virginica 
#> Levels: setosa versicolor virginica

# Data frame: sort by column name
sortX(d.frm, ord = "Species")
#>        Species Sepal.Length Sepal.Width
#> 5       setosa          5.0         3.6
#> 36      setosa          5.0         3.2
#> 20      setosa          5.1         3.8
#> 48      setosa          4.6         3.2
#> 74  versicolor          6.1         2.8
#> 140  virginica          6.9         3.1
#> 107  virginica          4.9         2.5
#> 136  virginica          7.7         3.0
#> 104  virginica          6.3         2.9
#> 146  virginica          6.7         3.0
sortX(d.frm, ord = c("Species", "Sepal.Length"))
#>        Species Sepal.Length Sepal.Width
#> 48      setosa          4.6         3.2
#> 5       setosa          5.0         3.6
#> 36      setosa          5.0         3.2
#> 20      setosa          5.1         3.8
#> 74  versicolor          6.1         2.8
#> 107  virginica          4.9         2.5
#> 104  virginica          6.3         2.9
#> 146  virginica          6.7         3.0
#> 140  virginica          6.9         3.1
#> 136  virginica          7.7         3.0

# Data frame: sort by column index
sortX(d.frm, ord = c(1L, 2L))
#>        Species Sepal.Length Sepal.Width
#> 48      setosa          4.6         3.2
#> 5       setosa          5.0         3.6
#> 36      setosa          5.0         3.2
#> 20      setosa          5.1         3.8
#> 74  versicolor          6.1         2.8
#> 107  virginica          4.9         2.5
#> 104  virginica          6.3         2.9
#> 146  virginica          6.7         3.0
#> 140  virginica          6.9         3.1
#> 136  virginica          7.7         3.0

# Decreasing order (per-column control)
sortX(d.frm, ord = c("Species", "Sepal.Length"),
      decreasing = c(FALSE, TRUE))
#>        Species Sepal.Length Sepal.Width
#> 20      setosa          5.1         3.8
#> 5       setosa          5.0         3.6
#> 36      setosa          5.0         3.2
#> 48      setosa          4.6         3.2
#> 74  versicolor          6.1         2.8
#> 136  virginica          7.7         3.0
#> 140  virginica          6.9         3.1
#> 146  virginica          6.7         3.0
#> 104  virginica          6.3         2.9
#> 107  virginica          4.9         2.5

# Natural sorting of character vectors
x <- c("A1", "A10", "A2")
sortX(x, method = "mixed")
#> [1] "A1"  "A2"  "A10"

# Factor: sort by label (default) vs. level order
sortX(d.frm, ord = "Species")                          # by label
#>        Species Sepal.Length Sepal.Width
#> 5       setosa          5.0         3.6
#> 36      setosa          5.0         3.2
#> 20      setosa          5.1         3.8
#> 48      setosa          4.6         3.2
#> 74  versicolor          6.1         2.8
#> 140  virginica          6.9         3.1
#> 107  virginica          4.9         2.5
#> 136  virginica          7.7         3.0
#> 104  virginica          6.3         2.9
#> 146  virginica          6.7         3.0
sortX(d.frm, ord = "Species", factorsAsCharacter = FALSE)  # by level
#>        Species Sepal.Length Sepal.Width
#> 5       setosa          5.0         3.6
#> 36      setosa          5.0         3.2
#> 20      setosa          5.1         3.8
#> 48      setosa          4.6         3.2
#> 74  versicolor          6.1         2.8
#> 140  virginica          6.9         3.1
#> 107  virginica          4.9         2.5
#> 136  virginica          7.7         3.0
#> 104  virginica          6.3         2.9
#> 146  virginica          6.7         3.0

# Tables: sort by column 2 descending
tab <- HairEyeColor[, , 1]
sortX(tab, ord = 2L, decreasing = TRUE)
#>        Eye
#> Hair    Brown Blue Hazel Green
#>   Brown    53   50    25    15
#>   Blond     3   30     5     8
#>   Black    32   11    10     3
#>   Red      10   10     7     7

# Tables: sort by marginal row sums
sortX(tab, ord = ncol(tab) + 1L, decreasing = TRUE)
#>        Eye
#> Hair    Brown Blue Hazel Green
#>   Brown    53   50    25    15
#>   Black    32   11    10     3
#>   Blond     3   30     5     8
#>   Red      10   10     7     7

# Sort by row names (always pass 0 as integer)
sortX(tab, ord = 0L)
#>        Eye
#> Hair    Brown Blue Hazel Green
#>   Black    32   11    10     3
#>   Blond     3   30     5     8
#>   Brown    53   50    25    15
#>   Red      10   10     7     7
```
