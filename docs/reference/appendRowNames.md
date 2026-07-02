# Append Rownames to a Data Frame or Matrix

Adds the row names of a data.frame or matrix as a column.

## Usage

``` r
appendRowNames(x, colName = "rowname", after = 0L, removeRownames = TRUE)
```

## Arguments

- x:

  A data.frame or matrix.

- colName:

  Name of the new column containing the row names.

- after:

  Position after which the column is inserted. Default is 0 (first
  column).

- removeRownames:

  Logical; if TRUE, existing row names are removed.

## Value

A data.frame (or matrix coerced to data.frame) with row names added as a
column.

## See also

[`append`](https://rdrr.io/r/base/append.html)

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendX()`](appendX.md), [`as.array.xtabs()`](as.array.xtabs.md),
[`asBinary()`](asBinary.md), [`asCDateFmt()`](asCDateFmt.md),
[`columnWrap()`](columnWrap.md), [`combLevels()`](combLevels.md),
[`compareDataFrames()`](compareDataFrames.md), [`dummy()`](dummy.md),
[`nf()`](nf.md), [`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`renameX()`](renameX.md), [`revCode()`](revCode.md),
[`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`splitAt()`](splitAt.md), [`splitX()`](splitX.md),
[`stringsAsFactors()`](stringsAsFactors.md), [`toBaseR()`](toBaseR.md)

## Examples

``` r
dd <- data.frame(x=1:5, y=6:10, z=LETTERS[1:5],
                 row.names = letters[1:5])
appendRowNames(dd)
#> $rowname
#> [1] "a" "b" "c" "d" "e"
#> 
#> $x
#> [1] 1 2 3 4 5
#> 
#> $y
#> [1]  6  7  8  9 10
#> 
#> $z
#> [1] "A" "B" "C" "D" "E"
#> 
```
