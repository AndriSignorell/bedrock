# Combine Levels from Multiple Inputs

Extracts and combines the levels from one or more vectors or factors.
Non-factor inputs are coerced to factors before extracting levels.

## Usage

``` r
combLevels(..., sort = FALSE, na = FALSE)
```

## Arguments

- ...:

  One or more vectors or factors.

- sort:

  Logical; if `TRUE`, the resulting levels are sorted.

- na:

  Logical; if `TRUE`, `NA` is treated as a valid level (i.e., included
  in the result).

## Value

A character vector containing the unique levels across all inputs.

## Details

Each input is coerced to a factor (if not already one), and its levels
are extracted. The union of all levels is returned.

By default, missing values (`NA`) are not included as a level. Set
`na = TRUE` to include them.

The order of levels follows their first occurrence unless `sort = TRUE`.

## See also

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md), [`asBinary()`](asBinary.md),
[`asCDateFmt()`](asCDateFmt.md), [`columnWrap()`](columnWrap.md),
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
x <- factor(c("A", "B"))
y <- c("B", "C")

combLevels(x, y)
#> [1] "A" "B" "C"

# Sorted levels
combLevels(x, y, sort = TRUE)
#> [1] "A" "B" "C"

# Including NA as a level
x <- c("A", NA)
y <- c("B", NA)
combLevels(x, y, na = TRUE)
#> [1] "A" NA  "B"
```
