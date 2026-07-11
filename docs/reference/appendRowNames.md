# Append Rownames to a Data Frame or Matrix

Adds the row names of a data.frame or matrix as a column.

## Usage

``` r
appendRowNames(x, colName = "rowname", after = 0L, removeRowNames = TRUE)
```

## Arguments

- x:

  a data.frame or matrix

- colName:

  name of the new column containing the row names

- after:

  position after which the column is inserted. Default is 0 (first
  column).

- removeRowNames:

  logical; if TRUE, existing row names are removed

## Value

An object of the same class as `x` with the row names added as a column.
Note that for matrices the result is coerced to the common mode, so
appending (character) row names to a numeric matrix yields a character
matrix.

## See also

[`append`](https://rdrr.io/r/base/append.html)

Other data.append: [`appendEnum()`](appendEnum.md),
[`appendX()`](appendX.md), [`multMerge()`](multMerge.md)

## Examples

``` r
dd <- data.frame(x = 1:5, y = 6:10, z = LETTERS[1:5],
                 row.names = letters[1:5])
appendRowNames(dd)
#>   rowname x  y z
#> 1       a 1  6 A
#> 2       b 2  7 B
#> 3       c 3  8 C
#> 4       d 4  9 D
#> 5       e 5 10 E
```
