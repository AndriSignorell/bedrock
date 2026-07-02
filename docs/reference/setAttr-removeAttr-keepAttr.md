# Set and Remove Object Attributes

Convenience helpers to add, remove, or selectively retain attributes of
an object.

## Usage

``` r
setAttr(x, attrNames, attrValues)

removeAttr(x, attrNames = NULL)

keepAttr(x, attrNames)
```

## Arguments

- x:

  Object to modify

- attrNames:

  Character vector of attribute names

- attrValues:

  Values for the attributes (only for setting)

## Value

Modified object

## See also

[`setNamesX`](setNamesX.md),
[`unname`](https://rdrr.io/r/base/unname.html)

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md), [`asBinary()`](asBinary.md),
[`asCDateFmt()`](asCDateFmt.md), [`columnWrap()`](columnWrap.md),
[`combLevels()`](combLevels.md),
[`compareDataFrames()`](compareDataFrames.md), [`dummy()`](dummy.md),
[`nf()`](nf.md), [`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`renameX()`](renameX.md), [`revCode()`](revCode.md),
[`revX()`](revX.md), [`setNamesX()`](setNamesX.md),
[`sortX()`](sortX.md), [`splitAt()`](splitAt.md),
[`splitX()`](splitX.md), [`stringsAsFactors()`](stringsAsFactors.md),
[`toBaseR()`](toBaseR.md)

## Examples

``` r
x <- runif(10)

x <- setAttr(
  x,
  attrNames = c("some_attr", "other_attr"),
  attrValues = c("First attribute", "Second attribute")
)

# remove single attribute
removeAttr(x, "other_attr")
#>  [1] 0.7179353 0.9614099 0.1001408 0.7632227 0.9479664 0.8186347 0.3082923
#>  [8] 0.6495795 0.9533555 0.9537327
#> attr(,"some_attr")
#> [1] "First attribute"

# remove all attributes
removeAttr(x)
#>  [1] 0.7179353 0.9614099 0.1001408 0.7632227 0.9479664 0.8186347 0.3082923
#>  [8] 0.6495795 0.9533555 0.9537327

# keep only selected attributes, remove all others
r.lm <- lm(Fertility ~ ., swiss)
keepAttr(r.lm$terms, "class")
#> Fertility ~ Agriculture + Examination + Education + Catholic + 
#>     Infant.Mortality
```
