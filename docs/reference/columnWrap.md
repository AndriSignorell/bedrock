# Column Wrap

Wraps text in a character matrix so that it's displayed over more than
one line.

## Usage

``` r
columnWrap(x, width = NULL)
```

## Arguments

- x:

  a character vector, typically one row of a matrix (e.g. via
  `apply(m, 1, columnWrap)`)

- width:

  integer, the width of the columns in characters, recycled to the
  length of `x`. Defaults to an equal share of `getOption("width")` per
  column.

## Value

a character matrix with one column per element of `x` and one row per
wrapped line

## Details

A data.frame containing character columns with long texts is often
wrapped by columns. This can lead to a loss of overview. `columnWrap()`
wraps the lines within the columns.

## See also

[`strwrap()`](https://rdrr.io/r/base/strwrap.html)

Other data.print: [`printCharMatrix()`](printCharMatrix.md)

## Examples

``` r

print(columnWrap("This is a very long text for a table", 12))
#>      [,1]        
#> [1,] "This is a" 
#> [2,] "very long" 
#> [3,] "text for a"
#> [4,] "table"     
```
