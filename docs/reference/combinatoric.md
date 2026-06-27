# Samples for Combinations of a Set

Return the value sets of combinations.

## Usage

``` r
combSet(x, m, replace = FALSE, ordered = FALSE, output = c("matrix", "list"))
```

## Arguments

- x:

  A vector of numeric values or characters. Character values need not be
  unique.

- m:

  Number of elements to choose. May be a vector.

- replace:

  Logical. Should repetition of the same element be allowed? Default is
  `FALSE`.

- ordered:

  Logical. Does the order matter? Default is `FALSE`.

- output:

  Character string specifying the output representation. One of:

  `\"matrix\"`

  :   Return combinations as a matrix.

  `\"list\"`

  :   Return combinations as a flat list.

  Default is `\"matrix\"`.

## Value

If `output = \"matrix\"`:

- a matrix with one combination per row

- if `length(m) > 1`, a list of matrices

If `output = \"list\"`:

- a flat list with one element per combination

## Details

Depending on `output`, the result is returned either as:

- a matrix with one combination per row

- a flat list where each element represents one combination

If `m` contains more than one value, combinations are generated
independently for each value of `m`.

## See also

[`combPairs`](combPairs.md),
[`combn`](https://rdrr.io/r/utils/combn.html),
[`choose`](https://rdrr.io/r/base/Special.html),
[`factorial`](https://rdrr.io/r/base/Special.html),
`vignette("Combinatorics")`

Other combinatorics: [`combN()`](combN.md),
[`combPairs()`](combPairs.md), [`permn()`](permn.md),
[`randGroupSplit()`](randGroupSplit.md), [`sampleX()`](sampleX.md)

## Examples

``` r
x <- letters[1:4]
m <- 2

# combinations with replacement
combSet(x, m, replace = TRUE, ordered = FALSE)
#>       [,1] [,2]
#>  [1,] "a"  "a" 
#>  [2,] "a"  "b" 
#>  [3,] "a"  "c" 
#>  [4,] "a"  "d" 
#>  [5,] "b"  "b" 
#>  [6,] "b"  "c" 
#>  [7,] "b"  "d" 
#>  [8,] "c"  "c" 
#>  [9,] "c"  "d" 
#> [10,] "d"  "d" 

# ordered combinations with replacement
combSet(x, m, replace = TRUE, ordered = TRUE)
#>       [,1] [,2]
#>  [1,] "a"  "a" 
#>  [2,] "b"  "a" 
#>  [3,] "c"  "a" 
#>  [4,] "d"  "a" 
#>  [5,] "a"  "b" 
#>  [6,] "b"  "b" 
#>  [7,] "c"  "b" 
#>  [8,] "d"  "b" 
#>  [9,] "a"  "c" 
#> [10,] "b"  "c" 
#> [11,] "c"  "c" 
#> [12,] "d"  "c" 
#> [13,] "a"  "d" 
#> [14,] "b"  "d" 
#> [15,] "c"  "d" 
#> [16,] "d"  "d" 

# ordered combinations without replacement
combSet(x, m, replace = FALSE, ordered = TRUE)
#>       [,1] [,2]
#>  [1,] "a"  "b" 
#>  [2,] "b"  "a" 
#>  [3,] "a"  "c" 
#>  [4,] "c"  "a" 
#>  [5,] "a"  "d" 
#>  [6,] "d"  "a" 
#>  [7,] "b"  "c" 
#>  [8,] "c"  "b" 
#>  [9,] "b"  "d" 
#> [10,] "d"  "b" 
#> [11,] "c"  "d" 
#> [12,] "d"  "c" 

# unordered combinations without replacement
combSet(x, m, replace = FALSE, ordered = FALSE)
#>      [,1] [,2]
#> [1,] "a"  "b" 
#> [2,] "a"  "c" 
#> [3,] "a"  "d" 
#> [4,] "b"  "c" 
#> [5,] "b"  "d" 
#> [6,] "c"  "d" 

# return as flat list
x <- letters[1:5]

combSet(
  x = x,
  m = c(1, 3, 5),
  output = "list"
)
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> [[3]]
#> [1] "c"
#> 
#> [[4]]
#> [1] "d"
#> 
#> [[5]]
#> [1] "e"
#> 
#> [[6]]
#> [1] "a" "b" "c"
#> 
#> [[7]]
#> [1] "a" "b" "d"
#> 
#> [[8]]
#> [1] "a" "b" "e"
#> 
#> [[9]]
#> [1] "a" "c" "d"
#> 
#> [[10]]
#> [1] "a" "c" "e"
#> 
#> [[11]]
#> [1] "a" "d" "e"
#> 
#> [[12]]
#> [1] "b" "c" "d"
#> 
#> [[13]]
#> [1] "b" "c" "e"
#> 
#> [[14]]
#> [1] "b" "d" "e"
#> 
#> [[15]]
#> [1] "c" "d" "e"
#> 
#> [[16]]
#> [1] "a" "b" "c" "d" "e"
#> 
```
