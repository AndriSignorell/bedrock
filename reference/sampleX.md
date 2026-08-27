# Random Samples and Permutations

`sampleX` takes a sample of the specified size from the elements of `x`,
with or without replacement. It does the same as
[`base::sample()`](https://rdrr.io/r/base/sample.html) and additionally
offers an interface for data frames, where rows are sampled.

## Usage

``` r
sampleX(x, size, replace = FALSE, prob = NULL)

# S3 method for class 'data.frame'
sampleX(x, size = nrow(x), replace = FALSE, prob = NULL)

# Default S3 method
sampleX(x, size, replace = FALSE, prob = NULL)
```

## Arguments

- x:

  either a vector of one or more elements from which to choose, or a
  positive integer, or a data frame whose rows are to be sampled.

- size:

  a non-negative integer giving the number of items (or rows) to choose.
  If missing, it defaults to the number of elements of `x` (resp.
  `nrow(x)` for data frames), yielding a random permutation.

- replace:

  logical; whether sampling is performed with replacement.

- prob:

  a vector of probability weights for obtaining the elements (or rows)
  being sampled.

## Value

sampled elements in the same structure as `x`; for data frames, a data
frame containing the sampled rows.

## See also

[`sample`](https://rdrr.io/r/base/sample.html)

Other combinatorics: [`combN()`](combN.md),
[`combPairs()`](combPairs.md), [`combSet()`](combSet.md),
[`pairApply()`](pairApply.md), [`permn()`](permn.md),
[`randGroupSplit()`](randGroupSplit.md)

## Examples

``` r

sampleX(1:10, size = 5)
#> [1] 10  7  4  3  5

# random permutation, like sample(x)
sampleX(1:10)
#>  [1]  2  1  3  8 10  6  9  5  7  4

# sample rows of a data frame
sampleX(mtcars, size = 5)
#>                mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Merc 450SE    16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Maserati Bora 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Duster 360    14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Camaro Z28    13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Toyota Corona 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
```
