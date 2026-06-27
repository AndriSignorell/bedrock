# Set of Permutations

Returns all distinct permutations of a vector. Repeated values in `x`
are treated as indistinguishable, so duplicated permutations are not
returned.

## Usage

``` r
permn(x, sortResults = FALSE)
```

## Arguments

- x:

  Atomic vector. Missing values are not supported.

- sortResults:

  Logical scalar. If `TRUE`, the result matrix is sorted using
  [`sortX()`](sortX.md). Default is `FALSE`.

## Value

A matrix containing all distinct permutations of `x`, one permutation
per row.

## See also

[`utils::combn()`](https://rdrr.io/r/utils/combn.html),
[`base::factorial()`](https://rdrr.io/r/base/Special.html),
[combPairs](combPairs.md)

Other combinatorics: [`combN()`](combN.md),
[`combPairs()`](combPairs.md), [`combSet()`](combinatoric.md),
[`randGroupSplit()`](randGroupSplit.md), [`sampleX()`](sampleX.md)

## Examples

``` r
permn(letters[2:5])
#>       [,1] [,2] [,3] [,4]
#>  [1,] "b"  "c"  "d"  "e" 
#>  [2,] "b"  "c"  "e"  "d" 
#>  [3,] "b"  "d"  "c"  "e" 
#>  [4,] "b"  "e"  "c"  "d" 
#>  [5,] "b"  "d"  "e"  "c" 
#>  [6,] "b"  "e"  "d"  "c" 
#>  [7,] "c"  "b"  "d"  "e" 
#>  [8,] "c"  "b"  "e"  "d" 
#>  [9,] "d"  "b"  "c"  "e" 
#> [10,] "e"  "b"  "c"  "d" 
#> [11,] "d"  "b"  "e"  "c" 
#> [12,] "e"  "b"  "d"  "c" 
#> [13,] "c"  "d"  "b"  "e" 
#> [14,] "c"  "e"  "b"  "d" 
#> [15,] "d"  "c"  "b"  "e" 
#> [16,] "e"  "c"  "b"  "d" 
#> [17,] "d"  "e"  "b"  "c" 
#> [18,] "e"  "d"  "b"  "c" 
#> [19,] "c"  "d"  "e"  "b" 
#> [20,] "c"  "e"  "d"  "b" 
#> [21,] "d"  "c"  "e"  "b" 
#> [22,] "e"  "c"  "d"  "b" 
#> [23,] "d"  "e"  "c"  "b" 
#> [24,] "e"  "d"  "c"  "b" 
permn(2:5)
#>       [,1] [,2] [,3] [,4]
#>  [1,]    2    3    4    5
#>  [2,]    2    3    5    4
#>  [3,]    2    4    3    5
#>  [4,]    2    5    3    4
#>  [5,]    2    4    5    3
#>  [6,]    2    5    4    3
#>  [7,]    3    2    4    5
#>  [8,]    3    2    5    4
#>  [9,]    4    2    3    5
#> [10,]    5    2    3    4
#> [11,]    4    2    5    3
#> [12,]    5    2    4    3
#> [13,]    3    4    2    5
#> [14,]    3    5    2    4
#> [15,]    4    3    2    5
#> [16,]    5    3    2    4
#> [17,]    4    5    2    3
#> [18,]    5    4    2    3
#> [19,]    3    4    5    2
#> [20,]    3    5    4    2
#> [21,]    4    3    5    2
#> [22,]    5    3    4    2
#> [23,]    4    5    3    2
#> [24,]    5    4    3    2

# repeated elements are handled as indistinguishable
permn(c("a", "b", "c", "a"))
#>       [,1] [,2] [,3] [,4]
#>  [1,] "a"  "a"  "b"  "c" 
#>  [2,] "a"  "a"  "c"  "b" 
#>  [3,] "a"  "b"  "a"  "c" 
#>  [4,] "a"  "c"  "a"  "b" 
#>  [5,] "a"  "b"  "c"  "a" 
#>  [6,] "a"  "c"  "b"  "a" 
#>  [7,] "b"  "a"  "a"  "c" 
#>  [8,] "c"  "a"  "a"  "b" 
#>  [9,] "b"  "a"  "c"  "a" 
#> [10,] "c"  "a"  "b"  "a" 
#> [11,] "b"  "c"  "a"  "a" 
#> [12,] "c"  "b"  "a"  "a" 
```
