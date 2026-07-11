# Compare Two Data Frames by Key Column

Compares two data frames row-by-row based on a key column, identifying
rows present in only one of the two frames and columns that differ in
matched rows.

## Usage

``` r
compareDataFrames(x, y, key)
```

## Arguments

- x:

  a data frame

- y:

  a data frame to compare against `x`

- key:

  character string. Name of the column used as row identifier. Must be
  present in both `x` and `y`, with unique values.

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

The values of the `key` column must be unique in both data frames.

## See also

Other data.equal: [`allDuplicated()`](allDuplicated.md),
[`allIdentical()`](allIdentical.md)

## Examples

``` r
x <- data.frame(id = c("A", "B", "C"), v1 = 1:3, v2 = c(10, 20, 30))
y <- data.frame(id = c("A", "B", "D"), v1 = c(1L, 9L, 4L), v2 = c(10, 20, 40))
compareDataFrames(x, y, key = "id")
#> $identical
#> [1] FALSE
#> 
#> $onlyInX
#>   id v1 v2
#> 3  C  3 30
#> 
#> $onlyInY
#>   id v1 v2
#> 3  D  4 40
#> 
#> $diffs
#>   id diffCols
#> 1  B       v1
#> 
```
