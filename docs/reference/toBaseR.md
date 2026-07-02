# Back to Basics with Tibbles

Sometimes we might wish for the old days be back and want to work with
familiar objects. This function helps to convert `tibbles` to
`data.frames` as smoothly as possible.

## Usage

``` r
toBaseR(x, ...)
```

## Arguments

- x:

  the object to be converted.

- ...:

  arguments passed on.

## Value

converted object

## See also

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
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
[`stringsAsFactors()`](stringsAsFactors.md)

## Examples

``` r

if (FALSE) { # \dontrun{
# read a Stata file
url <- "http://www.stata.com/videos13/data/webclass.dta"
d.webclass <- toBaseR(haven::read_dta(url))

# read a SPSS file
url <- "https://stats.idre.ucla.edu/wp-content/uploads/2020/10/missing.sav"
d.miss <- toBaseR(haven::read_sav(url))
} # }
```
