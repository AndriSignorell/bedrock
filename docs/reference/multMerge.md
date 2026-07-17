# Merge Multiple Data Frames

Merge multiple data frames by row names, or do other versions of
database join operations.

## Usage

``` r
multMerge(..., all.x = TRUE, all.y = TRUE, by = NULL)
```

## Arguments

- ...:

  data frames to be coerced to one.

- all.x:

  logical; if `TRUE`, then extra rows will be added to the output, one
  for each row in x that has no matching row in y. These rows will have
  `NA`s in those columns that are usually filled with values from y. The
  default is `TRUE`, so that non-matching rows are kept and padded with
  `NA`s (full outer join).

- all.y:

  logical; analogous to `all.x`.

- by:

  column used for merging, if this is not defined rownames will be used
  by default. The column must be included in all the provided data
  frames and its values must be unique within each data frame. Note that
  the restored key column is of type character.

## Value

a data frame. The rows are sorted according to the appearance of
previously unobserved rownames. So the rownames appearing in the first
data frame are first, then the rownames in the second data frame, which
have no corespondence in the first data frame and so on. The columns are
the remaining columns in x1 and then those in x2 and then those in x3.
The result has the row names resulting from the merge.

## See also

[`merge`](https://rdrr.io/r/base/merge.html)

Other data.append: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md)

## Examples

``` r

x1 <- setNamesX(data.frame(v=letters[1:6], w=1:6), 
                rownames=c("A", "B", "C", "D", "E", "F"))
x2 <- setNamesX(data.frame(v=letters[1:3], ww=11:13), 
                rownames=c("B", "C", "D"))
x3 <- setNamesX(data.frame(v=letters[12:16], wwww=22:26), 
                rownames=c("A", "C", "E", "G", "J"))

# default is "merge by rownames" 
multMerge(x1, x2, x3)
#>      v  w  v.1 ww  v.2 wwww
#> A    a  1 <NA> NA    l   22
#> B    b  2    a 11 <NA>   NA
#> C    c  3    b 12    m   23
#> D    d  4    c 13 <NA>   NA
#> E    e  5 <NA> NA    n   24
#> F    f  6 <NA> NA <NA>   NA
#> G <NA> NA <NA> NA    o   25
#> J <NA> NA <NA> NA    p   26
# ... which does not really make sense here

# merge by column v
multMerge(x1, x2, x3, by="v")
#>    v  w ww wwww
#> 1  a  1 11   NA
#> 2  b  2 12   NA
#> 3  c  3 13   NA
#> 4  d  4 NA   NA
#> 5  e  5 NA   NA
#> 6  f  6 NA   NA
#> 7  l NA NA   22
#> 8  m NA NA   23
#> 9  n NA NA   24
#> 10 o NA NA   25
#> 11 p NA NA   26
```
