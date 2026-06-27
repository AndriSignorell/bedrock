# Column Wrap

Wraps text in a character matrix so, that it's displayed over more than
one line.

## Usage

``` r
columnWrap(x, width = NULL)
```

## Arguments

- x:

  the matrix with one row

- width:

  integer, the width of the columns in characters

## Value

a character matrix

## Details

A data.frame containing character columns with long texts is often
wrapped by columns. This can lead to a loss of overview. `columnWrap()`
wraps the lines within the columns.

## See also

[`strwrap()`](https://rdrr.io/r/base/strwrap.html)

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md),
[`combLevels()`](combLevels.md), [`nf()`](nf.md),
[`parseSASDatalines()`](parseSASDatalines.md),
[`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`revCode()`](revCode.md), [`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`splitX()`](splitX.md), [`stringsAsFactors()`](stringsAsFactors.md),
[`toBaseR()`](toBaseR.md), [`untable()`](untable.md)

## Examples

``` r

print(columnWrap("This is a very long text for a table", 12))
#>      [,1]        
#> [1,] "This is a" 
#> [2,] "very long" 
#> [3,] "text for a"
#> [4,] "table"     
```
