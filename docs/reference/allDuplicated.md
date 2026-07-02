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

  A vector of any type.

## Value

A logical vector of the same length as `x`.

## Details

`allDuplicated` returns a logical vector indicating all elements of `x`
that are involved in ties (i.e., have frequency \> 1).

Note that `allDuplicated` flags all occurrences of tied values, not only
the duplicates beyond the first occurrence.

Consequently, `!allDuplicated(x)` can be used to identify elements of
`x` that appear exactly once.

## See also

[`duplicated`](https://rdrr.io/r/base/duplicated.html) for identifying
duplicate elements (excluding first occurrences).  
[`unique`](https://rdrr.io/r/base/unique.html) for extracting unique
values.  
[`split`](https://rdrr.io/r/base/split.html) for grouping tied values.  
[`table`](https://rdrr.io/r/base/table.html) for counting frequencies.  
[`union`](https://rdrr.io/r/base/sets.html),
[`intersect`](https://rdrr.io/r/base/sets.html),
[`setdiff`](https://rdrr.io/r/base/sets.html),
[`setequal`](https://rdrr.io/r/base/sets.html) for set-based operations
on vectors.

Other data.inspection: [`allIdentical()`](allIdentical.md),
[`completeColumns()`](completeColumns.md),
[`countCompCases()`](countCompCases.md), [`flags()`](flags.md),
[`isDichotomous()`](isDichotomous.md), [`isEuclid()`](isEuclid.md),
[`isNumeric()`](isNumeric.md), [`isURL()`](isURL.md),
[`isWholeLike()`](isWholeLike.md), [`isZero()`](isZero.md)

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

# Set operations
A <- c(sort(sample(1:20, 9)), NA)
B <- c(sort(sample(3:23, 7)), NA)

union(A, B)
#>  [1]  4  5  6  9 12 13 15 16 17 NA  7 23
intersect(A, B)
#> [1]  4  9 15 16 17 NA
setdiff(A, B)
#> [1]  5  6 12 13
setdiff(B, A)
#> [1]  7 23
setequal(A, B)
#> [1] FALSE

# Identify and analyse ties
x <- sample(letters[1:10], 20, replace = TRUE)
ties <- split(x, x)

# Number of tied groups
sum(sapply(ties, length) > 1)
#> [1] 5

# Sizes of tied groups
sizes <- sapply(ties, length)
sizes[sizes > 1]
#> b d e f i 
#> 3 2 4 4 2 

# Same via table()
tab <- table(x)
tab[tab > 1]
#> x
#> b d e f i 
#> 3 2 4 4 2 
```
