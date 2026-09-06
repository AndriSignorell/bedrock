# Resolve a Contingency Table

Brings a two-way classification into one canonical shape, no matter
whether it arrives as a ready-made contingency table or as two
classification variables. The function validates the counts, drops the
incomplete observations and reports the table together with its
dimensions, so that association measures, tests of independence and
agreement statistics can share one entry point instead of each repeating
the same preparation.

## Usage

``` r
resolveContingency(
  x,
  y = NULL,
  square = FALSE,
  integerCounts = TRUE,
  dataName = NULL
)
```

## Arguments

- x:

  a contingency table or matrix of counts, or a factor or vector of
  classifications.

- y:

  an optional factor or vector of classifications, of the same length as
  `x`. Required unless `x` is a table, ignored when it is.

- square:

  logical, whether a square contingency table is required, defaults to
  `FALSE`.

- integerCounts:

  logical, whether non-integer counts should be reported with a warning,
  defaults to `TRUE`.

- dataName:

  optional character string used as the `dataName` entry of the result.
  If `NULL` (default), it is derived from the unevaluated arguments.
  That name only reflects what `resolveContingency()` itself sees: a
  function calling it internally should build its own name from
  [`substitute()`](https://rdrr.io/r/base/substitute.html) at its own
  call site and pass it through here, as it would otherwise report its
  own formal argument names, typically `"x and y"`, instead of the names
  the end user typed.

## Value

a list containing:

- table:

  the contingency table.

- n:

  the total sample size, the sum of all counts.

- r:

  integer, the number of rows.

- c:

  integer, the number of columns.

- dataName:

  character description of the input, for use as the `data.name` of an
  `htest` object.

## Details

Any two-dimensional object is taken as a contingency table and used as
it is, which covers a matrix as well as a
[`table()`](https://rdrr.io/r/base/table.html) or
[`xtabs()`](https://rdrr.io/r/stats/xtabs.html) object; a data frame of
counts is coerced with
[`as.matrix()`](https://rdrr.io/r/base/matrix.html). Its entries must be
numeric, non-negative and finite; non-integer counts are reported with a
warning unless `integerCounts` is set to `FALSE`, as they occur
legitimately in weighted or expected tables. An array of any other
number of dimensions is an error, rather than being flattened into a
classification variable.

Two classification variables are cross-tabulated instead. Observations
missing in either variable are dropped, both variables are then coerced
to factors, which drops the levels that no longer occur, and at least
two levels must remain on each side.

Whichever way the table arrives, it must have at least two rows and two
columns: a one-way table carries no association to measure and is
rejected rather than passed on to a caller that cannot use it.

`square` is meant for the statistics that compare two ratings of the
same items, such as the tests of marginal homogeneity or the agreement
measures. It guarantees that the table has as many columns as rows, and
nothing beyond that: whether the two axes really carry the same
categories cannot be checked on a table that may have no `dimnames` at
all, and remains the responsibility of the caller.

## See also

[`table()`](https://rdrr.io/r/base/table.html),
[`resolveGroups()`](resolveGroups.md),
[`resolveFormula()`](resolveFormula.md)

Other data.resolve: [`resolveFormula()`](resolveFormula.md),
[`resolveGroups()`](resolveGroups.md)

## Examples

``` r
# from an existing contingency table
tab <- matrix(c(10, 5, 3, 12), nrow = 2,
              dimnames = list(c("A", "B"), c("yes", "no")))
str(resolveContingency(tab))
#> List of 5
#>  $ table   : num [1:2, 1:2] 10 5 3 12
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:2] "A" "B"
#>   .. ..$ : chr [1:2] "yes" "no"
#>  $ n       : num 30
#>  $ r       : int 2
#>  $ c       : int 2
#>  $ dataName: chr "tab"

# from two classification variables
set.seed(1)
x <- sample(c("low", "high"), 100, replace = TRUE)
y <- sample(c("yes", "no"), 100, replace = TRUE)
resolveContingency(x, y)$table
#>       y
#> x      no yes
#>   high 23  28
#>   low  24  25

# a caller passes the name it sees at its own call site
myTest <- function(x, y) {
  r <- resolveContingency(x, y,
                          dataName = paste(deparse1(substitute(x)), "and",
                                           deparse1(substitute(y))))
  r$dataName
}
myTest(x, y)
#> [1] "x and y"
## [1] "x and y"
```
