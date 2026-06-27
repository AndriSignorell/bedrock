# Resolve Contingency Table

Converts either a contingency table or two classification variables into
a standardized contingency-table representation.

## Usage

``` r
resolveContingency(x, y = NULL, square = FALSE, integerCounts = TRUE)
```

## Arguments

- x:

  a contingency table, factor or vector.

- y:

  an optional factor or vector. Ignored when `x` is a matrix.

- square:

  logical indicating whether a square contingency table is required.

- integerCounts:

  logical; if `TRUE` (default) a warning is issued when the table
  contains non-integer counts.

## Value

A list containing:

- table:

  contingency table

- n:

  total sample size

- r:

  number of rows

- c:

  number of columns

- k:

  number of rows (alias; convenient for square tables)

- data.name:

  name of the data

## Details

If `x` is a matrix, it is interpreted as a contingency table. Otherwise
`x` and `y` are converted to factors and a table is constructed after
removing incomplete observations.

## See also

Other data.utils: [`resolveGroups()`](resolveGroups.md)
