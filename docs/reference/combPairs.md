# Get All Pairs Out of One or Two Sets of Elements

Returns all combinations of 2 out of the elements in x or x and y (if
defined). Combinations of the same elements will be dropped (no
replacing). The vector `x` need not contain unique values. Duplicate
elements in `x` will result in duplicate pairs.

## Usage

``` r
combPairs(x, y = NULL)
```

## Arguments

- x:

  a vector of elements, must contain at least 2 elements if `y` is
  `NULL`.

- y:

  a vector of elements, need not be same dimension as x. If y is not
  `NULL` then all combination x and y are returned.

## Value

a data.frame with two columns `X1` and `X2` containing the pairwise
combinations.

## Details

If y = `NULL` then all combination of 2 out of x are returned.  
If y is defined then all combinations of x and y are calculated.

## See also

[`combn`](https://rdrr.io/r/utils/combn.html),
[`expand.grid`](https://rdrr.io/r/base/expand.grid.html),
[`outer`](https://rdrr.io/r/base/outer.html),
[`lower.tri`](https://rdrr.io/r/base/lower.tri.html)

Other combinatorics: [`combN()`](combN.md), [`combSet()`](combSet.md),
[`pairApply()`](pairApply.md), [`permn()`](permn.md),
[`randGroupSplit()`](randGroupSplit.md), [`sampleX()`](sampleX.md)

## Examples

``` r

combPairs(letters[1:4])
#>   X1 X2
#> 1  a  b
#> 2  a  c
#> 3  a  d
#> 4  b  c
#> 5  b  d
#> 6  c  d
combPairs(x = letters[1:4], y = LETTERS[1:2])
#>   X1 X2
#> 1  a  A
#> 2  b  A
#> 3  c  A
#> 4  d  A
#> 5  a  B
#> 6  b  B
#> 7  c  B
#> 8  d  B

# get all pairs of combinations between factors and numerics out of a data.frame
combPairs(which(sapply(CO2, is.numeric)), which(sapply(CO2, is.factor)))
#>   X1 X2
#> 1  4  1
#> 2  5  1
#> 3  4  2
#> 4  5  2
#> 5  4  3
#> 6  5  3
```
