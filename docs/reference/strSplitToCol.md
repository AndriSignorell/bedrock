# Split Strings into Multiple Columns

Splits character vectors into multiple columns based on a delimiter.
Each element of `x` is split using
[`strsplit`](https://rdrr.io/r/base/strsplit.html), and the resulting
parts are expanded into separate columns.

## Usage

``` r
strSplitToCol(x, split = " ", fixed = TRUE, naForm = "", colNames = NULL)
```

## Arguments

- x:

  A character vector or a data frame of character columns to be split.
  Each element (or column) is processed separately.

- split:

  Character string specifying the delimiter for splitting. Passed to
  [`strsplit`](https://rdrr.io/r/base/strsplit.html).

- fixed:

  Logical; if `TRUE`, `split` is used as a fixed string. Otherwise, it
  is treated as a regular expression.

- naForm:

  Character value used to replace missing elements created by unequal
  split lengths.

- colNames:

  Optional character vector specifying column names for the resulting
  data frame. Recycled if necessary.

## Value

A data frame containing the split components of `x`. Additional
attribute:

- `cols`: Integer vector with number of columns per input element

## Details

All rows are padded to the same number of columns per input element.
Missing values are filled with `naForm`.

For each element (or column) in `x`, the function:

1.  Splits each entry using
    [`strsplit`](https://rdrr.io/r/base/strsplit.html)

2.  Determines the maximum number of split parts

3.  Pads shorter splits with `naForm`

4.  Combines results into a matrix via
    [`rbind`](https://rdrr.io/r/base/cbind.html)

The final result is a data frame where each original element or column
contributes one or more columns depending on the number of splits.

An attribute `"cols"` is attached, indicating the number of columns
generated for each element of `x`.

## See also

Other string.utilities:
[`char-ascii-conversion`](char-ascii-conversion.md),
[`mGsub()`](mGsub.md), [`mReplace()`](mReplace.md),
[`strSplitToDummy()`](strSplitToDummy.md)

## Examples

``` r
x <- c("A B C", "D E", "F")
strSplitToCol(x)
#>   X1 X2 X3
#> 1  A  B  C
#> 2  D  E   
#> 3  F      

# Custom delimiter
x <- c("A|B|C", "D|E", "F")
strSplitToCol(x, split = "|")
#>   X1 X2 X3
#> 1  A  B  C
#> 2  D  E   
#> 3  F      

# Multiple columns
df <- data.frame(
  a = c("x y", "z"),
  b = c("1 2 3", "4 5"),
  stringsAsFactors = FALSE
)
strSplitToCol(df)
#>   a.1 a.2 b.1 b.2 b.3
#> 1   x   y   1   2   3
#> 2   z       4   5    
```
