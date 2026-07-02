# Append Elements to Objects

Generic function to append elements to vectors, matrices, and data
frames.

## Usage

``` r
appendX(x, values, after = NULL, ...)

# Default S3 method
appendX(x, values, after = NULL, ...)

# S3 method for class 'matrix'
appendX(x, values, after = NULL, rows = FALSE, newNames = NULL, ...)

# S3 method for class 'data.frame'
appendX(x, values, after = NULL, rows = FALSE, newNames = NULL, ...)

# S3 method for class 'TOne'
appendX(x, values, after = NULL, rows = TRUE, newNames = NULL, ...)
```

## Arguments

- x:

  Object to which values are appended.

- values:

  Values to insert into `x`.

- after:

  Position after which to insert. If `NULL`, values are appended at the
  end. Use `0` to prepend.

- ...:

  Additional arguments.

- rows:

  Logical; if TRUE, insert rows instead of columns. Ignored for vectors.

- newNames:

  Optional names for inserted elements.

## Value

Object of same type as `x`.

## See also

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md),
[`as.array.xtabs()`](as.array.xtabs.md), [`asBinary()`](asBinary.md),
[`asCDateFmt()`](asCDateFmt.md), [`columnWrap()`](columnWrap.md),
[`combLevels()`](combLevels.md),
[`compareDataFrames()`](compareDataFrames.md), [`dummy()`](dummy.md),
[`nf()`](nf.md), [`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`renameX()`](renameX.md), [`revCode()`](revCode.md),
[`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`splitAt()`](splitAt.md), [`splitX()`](splitX.md),
[`stringsAsFactors()`](stringsAsFactors.md), [`toBaseR()`](toBaseR.md)
