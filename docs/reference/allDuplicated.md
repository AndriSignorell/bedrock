# Logical Indicator for All Values Involved in Ties

The function [`duplicated`](https://rdrr.io/r/base/duplicated.html)
returns a logical vector indicating which elements of `x` are
duplicates, but it does not flag the first occurrence of subsequently
duplicated elements.

## Usage

``` r
allDuplicated(x)
```

## Arguments

- x:

  a vector of any type.

## Value

a logical vector of the same length as `x`.

## Details

`allDuplicated` returns a logical vector indicating all elements of `x`
that are involved in ties (i.e., have frequency \> 1).

Note that `allDuplicated` flags all occurrences of tied values, not only
the duplicates beyond the first occurrence.

Consequently, `!allDuplicated(x)` can be used to identify elements of
`x` that appear exactly once.

Missing values are considered equal to each other, so multiple `NA`s are
flagged as ties. As the function builds on
[`duplicated`](https://rdrr.io/r/base/duplicated.html), it also works
for data frames (row-wise) and matrices.

## See also

[`duplicated`](https://rdrr.io/r/base/duplicated.html) for identifying
duplicate elements (excluding first occurrences).  
[`unique`](https://rdrr.io/r/base/unique.html) for extracting unique
values.  
[`split`](https://rdrr.io/r/base/split.html) for grouping tied values.  
[`table`](https://rdrr.io/r/base/table.html) for counting frequencies.

Other data.equal: [`allIdentical()`](allIdentical.md),
[`compareDataFrames()`](compareDataFrames.md)

## Examples

``` r
x <- c(1:10, 4:6)

allDuplicated(x)
#>  [1] FALSE FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
#> [13]  TRUE

# Compare with duplicated():
duplicated(x)
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE
#> [13]  TRUE

# Elements appearing exactly once
x[!allDuplicated(x)]
#> [1]  1  2  3  7  8  9 10

# Identify and analyse ties
x <- sample(letters[1:10], 20, replace = TRUE)
ties <- split(x, x)

# Number of tied groups
sum(sapply(ties, length) > 1)
#> [1] 6

# Sizes of tied groups
sizes <- sapply(ties, length)
sizes[sizes > 1]
#> b d e f g i 
#> 3 2 5 4 2 2 

# Same via table()
tab <- table(x)
tab[tab > 1]
#> x
#> b d e f g i 
#> 3 2 5 4 2 2 

```
