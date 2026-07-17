# Reshape Between Long and Wide Format

Reshape data between long and wide format using a grouping variable.

## Usage

``` r
toLong(x, varNames = NULL, includeRowNames = FALSE)

toWide(x, groups, by = NULL, varNames = NULL)
```

## Arguments

- x:

  object to reshape. For `toLong()`, a matrix, table, data frame, or
  list. For `toWide()`, a vector.

- varNames:

  optional character vector of column names for the result.

- includeRowNames:

  logical. If `TRUE`, append a column containing the row names of `x`
  when reshaping to long format.

- groups:

  grouping vector used to define the columns in the wide result.

- by:

  optional vector used to align values row-wise when reshaping to wide
  format. If `NULL`, values are aligned by their order within each
  group.

## Value

a reshaped object of class `data.frame`.

## Details

`toLong()` expects `x` to be a matrix, table, data frame, or list and
reshapes it to a long data frame representation. `toWide()` expects a
vector `x` and a grouping vector `groups`, and reshapes the values into
one column per group.

## See also

`reshape`, `stack`, `unstack`

Other data.reshape: [`collapseTable()`](collapseTable.md),
[`splitAt()`](splitAt.md), [`splitX()`](splitX.md),
[`untable()`](untable.md)

## Examples

``` r
d.x <- read.table(header = TRUE, text = "
AA BB CC DD EE FF GG
7.9 18.1 13.3 6.2 9.3 8.3 10.6
9.8 14.0 13.6 7.9 2.9 9.1 13.0
6.4 17.4 16.0 10.9 8.6 11.7 17.5
")

toLong(d.x)
#>      groups    x
#> 1.AA     AA  7.9
#> 2.AA     AA  9.8
#> 3.AA     AA  6.4
#> 1.BB     BB 18.1
#> 2.BB     BB 14.0
#> 3.BB     BB 17.4
#> 1.CC     CC 13.3
#> 2.CC     CC 13.6
#> 3.CC     CC 16.0
#> 1.DD     DD  6.2
#> 2.DD     DD  7.9
#> 3.DD     DD 10.9
#> 1.EE     EE  9.3
#> 2.EE     EE  2.9
#> 3.EE     EE  8.6
#> 1.FF     FF  8.3
#> 2.FF     FF  9.1
#> 3.FF     FF 11.7
#> 1.GG     GG 10.6
#> 2.GG     GG 13.0
#> 3.GG     GG 17.5

# to wide by row order
toWide(PlantGrowth$weight, PlantGrowth$group)
#>    ctrl trt1 trt2
#> 1  4.17 4.81 6.31
#> 2  5.58 4.17 5.12
#> 3  5.18 4.41 5.54
#> 4  6.11 3.59 5.50
#> 5  4.50 5.87 5.37
#> 6  4.61 3.83 5.29
#> 7  5.17 6.03 4.92
#> 8  4.53 4.89 6.15
#> 9  5.33 4.32 5.80
#> 10 5.14 4.69 5.26

# to wide aligned by key
set.seed(41)
PlantGrowth$nr <- c(sample(12, 10), sample(12, 10), sample(12, 10))
toWide(PlantGrowth$weight, PlantGrowth$group, by = PlantGrowth$nr)
#>    by ctrl trt1 trt2
#> 1   1   NA 4.89 5.80
#> 2   2 4.50 4.17 5.12
#> 3   3 5.58   NA 5.26
#> 4   4   NA 6.03 6.31
#> 5   5 5.18 4.41   NA
#> 6   6 5.17 4.81 5.50
#> 7   7 5.33 4.69   NA
#> 8   8 4.17 5.87 5.29
#> 9   9 4.53 4.32 6.15
#> 10 10 4.61 3.83 4.92
#> 11 11 5.14   NA 5.37
#> 12 12 6.11 3.59 5.54
```
