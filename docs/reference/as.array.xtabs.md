# Coerce `xtabs` Object to Array or Matrix

Converts an object of class `"xtabs"` to a plain matrix by dropping all
additional classes such as `"xtabs"` and `"table"`.

## Usage

``` r
# S3 method for class 'xtabs'
as.array(x, ...)
```

## Arguments

- x:

  An object of class `"xtabs"`.

- ...:

  Ignored.

## Value

A matrix with no additional classes.

## See also

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`columnWrap()`](columnWrap.md), [`combLevels()`](combLevels.md),
[`nf()`](nf.md), [`parseSASDatalines()`](parseSASDatalines.md),
[`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`revCode()`](revCode.md), [`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`splitX()`](splitX.md), [`stringsAsFactors()`](stringsAsFactors.md),
[`toBaseR()`](toBaseR.md), [`untable()`](untable.md)

## Examples

``` r
xt <- xtabs(~ cyl + gear, data = mtcars)
class(as.matrix(xt))
#> [1] "matrix" "array" 
# "matrix"
```
