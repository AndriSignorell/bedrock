# Convert to Numeric via Factor

Converts an object to numeric by first coercing it to a factor and then
to numeric.

## Usage

``` r
nf(x, ...)
```

## Arguments

- x:

  A vector to be converted.

- ...:

  Additional arguments passed to
  [`factor`](https://rdrr.io/r/base/factor.html).

## Value

A numeric vector corresponding to the integer codes of the factor
levels.

## Details

This function is a shorthand for `as.numeric(factor(x, ...))`. It is
useful for converting categorical or character variables into integer
codes representing factor levels.

Note that the resulting numeric values correspond to the internal factor
levels, not the original numeric values.

## See also

[`factor`](https://rdrr.io/r/base/factor.html),
[`as.numeric`](https://rdrr.io/r/base/numeric.html)

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md), [`asBinary()`](asBinary.md),
[`asCDateFmt()`](asCDateFmt.md), [`columnWrap()`](columnWrap.md),
[`combLevels()`](combLevels.md),
[`compareDataFrames()`](compareDataFrames.md), [`dummy()`](dummy.md),
[`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`renameX()`](renameX.md), [`revCode()`](revCode.md),
[`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`splitAt()`](splitAt.md), [`splitX()`](splitX.md),
[`stringsAsFactors()`](stringsAsFactors.md), [`toBaseR()`](toBaseR.md)

## Examples

``` r
nf(c("a", "b", "a"))
#> [1] 1 2 1
nf(c("low", "medium", "high"), levels = c("low", "medium", "high"))
#> [1] 1 2 3
```
