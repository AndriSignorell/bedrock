# Compare Two Data Frames by Key Column

Compares two data frames row-by-row based on a key column, identifying
rows present in only one of the two frames and columns that differ in
matched rows.

## Usage

``` r
compareDataFrames(x, y, key = "strat_x")
```

## Arguments

- x:

  A data frame.

- y:

  A data frame to compare against `x`.

- key:

  Character string. Name of the column used as row identifier. Must be
  present in both `x` and `y`. Default is `"strat_x"`.

## Value

A named list with four elements:

- `identical`:

  Logical. `TRUE` if the two data frames are identical with respect to
  the common columns and key.

- `onlyInX`:

  Data frame of rows whose key value appears in `x` but not in `y`.

- `onlyInY`:

  Data frame of rows whose key value appears in `y` but not in `x`.

- `diffs`:

  Data frame with columns named after the `key` argument (the key value)
  and `diffCols` (a list column of character vectors naming the
  differing columns for that key).

## Details

Only columns present in both data frames are compared. Rows are matched
by the `key` column using
[`identical`](https://rdrr.io/r/base/identical.html) for element-wise
comparison, so type differences (e.g., `integer` vs. `double`) will be
flagged as differences.

## See also

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md), [`asBinary()`](asBinary.md),
[`asCDateFmt()`](asCDateFmt.md), [`columnWrap()`](columnWrap.md),
[`combLevels()`](combLevels.md), [`dummy()`](dummy.md), [`nf()`](nf.md),
[`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`renameX()`](renameX.md), [`revCode()`](revCode.md),
[`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`splitAt()`](splitAt.md), [`splitX()`](splitX.md),
[`stringsAsFactors()`](stringsAsFactors.md), [`toBaseR()`](toBaseR.md)

## Examples

``` r
x <- data.frame(strat_x = c("A", "B", "C"), v1 = 1:3, v2 = c(10, 20, 30))
y <- data.frame(strat_x = c("A", "B", "D"), v1 = c(1L, 9L, 4L), v2 = c(10, 20, 40))
compareDataFrames(x, y)
#> $identical
#> [1] FALSE
#> 
#> $onlyInX
#>   strat_x v1 v2
#> 3       C  3 30
#> 
#> $onlyInY
#>   strat_x v1 v2
#> 3       D  4 40
#> 
#> $diffs
#>   strat_x diffCols
#> 1       B       v1
#> 
```
