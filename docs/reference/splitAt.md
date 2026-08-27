# Split a Vector at Given Positions

Splits a vector into consecutive segments at specified positions.

## Usage

``` r
splitAt(x, pos)
```

## Arguments

- x:

  a vector to be split.

- pos:

  an integer vector of positions at which to split `x`. Positions refer
  to indices in `x` where a new segment should start.

## Value

a list of vectors, each representing a segment of `x`.

## Details

The function splits `x` into consecutive chunks defined by `pos`.
Internally, positions are sorted, duplicates are removed, and values
that would not produce a non-empty segment (`pos < 2` or
`pos > length(x)`) are ignored. Empty segments are never returned.

Each element of the returned list corresponds to a contiguous subset of
`x`. The first segment always starts at position 1.

## See also

[`split`](https://rdrr.io/r/base/split.html)

Other data.reshape: [`collapseTable()`](collapseTable.md),
[`long-wide-reshape`](long-wide-reshape.md), [`splitX()`](splitX.md),
[`untable()`](untable.md)

## Examples

``` r
x <- 1:10

# split at positions 4 and 7
splitAt(x, c(4, 7))
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 4 5 6
#> 
#> [[3]]
#> [1]  7  8  9 10
#> 

# unsorted and duplicate positions are handled
splitAt(x, c(7, 4, 4, 20))
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 4 5 6
#> 
#> [[3]]
#> [1]  7  8  9 10
#> 
```
