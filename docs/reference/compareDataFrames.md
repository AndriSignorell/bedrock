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

- `only_in_x`:

  Data frame of rows whose key value appears in `x` but not in `y`.

- `only_in_y`:

  Data frame of rows whose key value appears in `y` but not in `x`.

- `diffs`:

  Data frame with columns `strat_x` (the key value) and `diff_cols` (a
  list column of character vectors naming the differing columns for that
  key).

## Details

Only columns present in both data frames are compared. Rows are matched
by the `key` column using
[`identical`](https://rdrr.io/r/base/identical.html) for element-wise
comparison, so type differences (e.g., `integer` vs. `double`) will be
flagged as differences.

## Examples

``` r
x <- data.frame(strat_x = c("A", "B", "C"), v1 = 1:3, v2 = c(10, 20, 30))
y <- data.frame(strat_x = c("A", "B", "D"), v1 = c(1L, 9L, 4L), v2 = c(10, 20, 40))
compareDataFrames(x, y)
#> $identical
#> [1] FALSE
#> 
#> $only_in_x
#>   strat_x v1 v2
#> 3       C  3 30
#> 
#> $only_in_y
#>   strat_x v1 v2
#> 3       D  4 40
#> 
#> $diffs
#>   strat_x diff_cols
#> 1       B        v1
#> 
```
