# Add an enumeration column

Prepends (or inserts) a column of enumeration labels – lowercase or
uppercase Roman numerals, or Arabic numbers.

## Usage

``` r
appendEnum(
  x,
  type = c("roman-lcase", "roman-ucase", "arabic"),
  suffix = ". ",
  startWith = 1L,
  after = 0L,
  colName = NULL
)
```

## Arguments

- x:

  a data.frame or matrix (vectors are coerced via
  [`matrix()`](https://rdrr.io/r/base/matrix.html))

- type:

  enumeration style: `"roman-lcase"`, `"roman-ucase"` or `"arabic"`

- suffix:

  text appended to each enumeration label

- startWith:

  first enumeration index

- after:

  position after which the column is inserted (see
  [`appendX`](appendX.md)); default `0L` prepends it

- colName:

  optional name for the new column; `NULL` (default) leaves it unnamed

## Value

`x` with an additional enumeration column

## See also

Other data.manipulation: [`appendRowNames()`](appendRowNames.md),
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
