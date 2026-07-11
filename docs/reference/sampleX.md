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

  logical. Should sampling be with replacement?

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
#> [1] 10  8  2  5  4

# random permutation, like sample(x)
sampleX(1:10)
#>  [1]  7  8  5  4  6  1  3  2  9 10

# sample rows of a data frame
sampleX(mtcars, size = 5)
#>                   mpg cyl  disp  hp drat   wt  qsec vs am gear carb
#> Mazda RX4        21.0   6 160.0 110 3.90 2.62 16.46  0  1    4    4
#> Ford Pantera L   15.8   8 351.0 264 4.22 3.17 14.50  0  1    5    4
#> Merc 450SLC      15.2   8 275.8 180 3.07 3.78 18.00  0  0    3    3
#> Dodge Challenger 15.5   8 318.0 150 2.76 3.52 16.87  0  0    3    2
#> Valiant          18.1   6 225.0 105 2.76 3.46 20.22  1  0    3    1
```
