# Resolve Contingency Table

Converts either a contingency table or two classification variables into
a standardized contingency-table representation.

## Usage

``` r
resolveContingency(
  x,
  y = NULL,
  square = FALSE,
  integerCounts = TRUE,
  data.name = NULL
)
```

## Arguments

- x:

  a contingency table, factor or vector.

- y:

  an optional factor or vector Ignored when `x` is a matrix.

- square:

  logical indicating whether a square contingency table is required.

- integerCounts:

  logical; if `TRUE` (default) a warning is issued when the table
  contains non-integer counts.

- data.name:

  optional character string used as the `data.name` entry of the result.
  If `NULL` (default), it is derived from `deparse(substitute(x))` (and
  `y`). This only reflects the variable names as seen by
  `resolveContingency()` itself: functions that call
  `resolveContingency()` internally should build their own `data.name`
  via `deparse(substitute())` at their own call site and pass it through
  here, otherwise the reported name will be the formal argument names of
  the calling function (e.g. `"x and y"`) rather than the names the end
  user actually typed.

## Value

a list containing:

- table:

  contingency table.

- n:

  total sample size.

- r:

  number of rows.

- c:

  number of columns.

- k:

  number of rows (alias; convenient for square tables).

- data.name:

  name of the data.

## Details

If `x` is a matrix, it is interpreted as a contingency table. Otherwise
`x` and `y` are converted to factors and a table is constructed after
removing incomplete observations.

## See also

Other data.resolve: [`resolveFormula()`](resolveFormula.md),
[`resolveGroups()`](resolveGroups.md)

## Examples

``` r
# from an existing contingency table
tab <- matrix(c(10, 5, 3, 12), nrow = 2,
              dimnames = list(c("A", "B"), c("yes", "no")))
resolveContingency(tab)
#> $table
#>   yes no
#> A  10  3
#> B   5 12
#> 
#> $n
#> [1] 30
#> 
#> $r
#> [1] 2
#> 
#> $c
#> [1] 2
#> 
#> $k
#> [1] 2
#> 
#> $data.name
#> [1] "tab"
#> 

# from two classification variables
set.seed(1)
x <- sample(c("low", "high"), 100, replace = TRUE)
y <- sample(c("yes", "no"), 100, replace = TRUE)
resolveContingency(x, y)
#> $table
#>       y
#> x      no yes
#>   high 23  28
#>   low  24  25
#> 
#> $n
#> [1] 100
#> 
#> $r
#> [1] 2
#> 
#> $c
#> [1] 2
#> 
#> $k
#> [1] 2
#> 
#> $data.name
#> [1] "x and y"
#> 
```
