# Collapse Table Dimensions by Remapping Factor Levels

Aggregates a `table` or `ftable` object by reassigning the levels of one
or more dimensions according to user-supplied mappings, and summing the
frequencies within each resulting level combination.

## Usage

``` r
collapseTable(x, ..., strict = TRUE)
```

## Arguments

- x:

  a `table` or `ftable` object with named dimensions

- ...:

  named or unnamed mapping vectors specifying how levels of each
  dimension should be collapsed. Each mapping vector must have length
  equal to the number of levels in the corresponding dimension.

- strict:

  logical (default `TRUE`). Controls error handling.

## Value

A collapsed `table` object with updated dimensions and aggregated
frequencies.

## Details

Mapping vectors define how factor levels are reassigned. Element `i`
specifies the new label for the `i`-th original level. Repeated values
in a mapping vector cause the corresponding levels to be merged.

**Argument matching**

- Named arguments are matched to dimensions by name (e.g.,
  `age = c("young", "adult")`).

- Unnamed arguments are assigned to dimensions in order.

- Mixed usage assigns named arguments first, then remaining unnamed
  arguments in order.

**Constraints**

- Each dimension may be specified at most once.

- Mapping vectors must have the same length as the number of levels of
  the corresponding dimension.

- `NA` values in mapping vectors are not allowed.

**Level ordering** The order of the resulting levels follows the first
occurrence of each value in the mapping vector.

**Error handling**

- If `strict = TRUE`, unknown dimension names result in an error, and
  positional assignment of unnamed arguments produces a warning.

- If `strict = FALSE`, unknown dimensions produce a warning (and are
  ignored), and positional assignment is silent.

## See also

Other data.reshape: [`long-wide-reshape`](long-wide-reshape.md),
[`splitAt()`](splitAt.md), [`splitX()`](splitX.md),
[`untable()`](untable.md)

## Examples

``` r
tab <- xtabs(~ gear + cyl, data = mtcars)
tab
#>     cyl
#> gear  4  6  8
#>    3  1  2 12
#>    4  8  4  0
#>    5  2  1  2

# merge the 4- and 6-cylinder categories
collapseTable(tab, cyl = c("le6", "le6", "8"))
#>     cyl
#> gear le6  8
#>    3   3 12
#>    4  12  0
#>    5   3  2

# positional assignment (first dimension), silent with strict = FALSE
collapseTable(tab, c("3-4", "3-4", "5"), strict = FALSE)
#>      cyl
#> gear   4  6  8
#>   3-4  9  6 12
#>   5    2  1  2
```
