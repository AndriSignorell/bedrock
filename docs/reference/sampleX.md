# Random Samples and Permutations

`Sample` takes a sample of the specified size from the elements of x
using either with or without replacement. The function does the same as
the base::sample() and offers additionally an interface for data frames.

## Usage

``` r
sampleX(x, size, replace = FALSE, prob = NULL)

# S3 method for class 'data.frame'
sampleX(x, size, replace = FALSE, prob = NULL)

# Default S3 method
sampleX(x, size, replace = FALSE, prob = NULL)
```

## Arguments

- x:

  either a vector of one or more elements from which to choose, or a
  positive integer.

- size:

  a positive number, the number of items to choose from.

- replace:

  a non-negative integer giving the number of items to choose.

- prob:

  should sampling be with replacement?

## Value

sampled elements in the same structure as x

## See also

[`sample`](https://rdrr.io/r/base/sample.html)

Other combinatorics: [`combN()`](combN.md),
[`combPairs()`](combPairs.md), [`combSet()`](combinatoric.md),
[`permn()`](permn.md), [`randGroupSplit()`](randGroupSplit.md)

## Examples

``` r

sampleX(Pizza, size=5)
#>      index       date week weekday        area count rabate price operator
#> 1148  1148 2014-03-30   13       7      Camden     3  FALSE 43.97   Rhonda
#> 1101  1101 2014-03-29   13       6 Westminster     3  FALSE 47.97    Maria
#> 19      19 2014-03-01    9       6       Brent     3  FALSE 41.97   Rhonda
#> 273    273 2014-03-08   10       6      Camden     3  FALSE 42.97  Allanah
#> 418    418 2014-03-12   11       3       Brent     3  FALSE 59.66  Allanah
#>      driver delivery_min temperature wine_ordered wine_delivered wrongpizza
#> 1148 Carter         24.0        53.4            0              0      FALSE
#> 1101 Hunter         53.4        20.2            0              0      FALSE
#> 19   Carter          9.1        52.8            0              0      FALSE
#> 273  Taylor         40.4        21.7            0              0      FALSE
#> 418  Carter         11.2        58.3            1              1      FALSE
#>      quality vegetarian nps complaint    style channel  tip
#> 1148  medium          0   8         1  italian     app 0.00
#> 1101  medium          1   5         0  italian     app 0.00
#> 19    medium          0   9         0    vegan     web 4.58
#> 273      low          0   3        NA  gourmet     app 0.00
#> 418     high          0   8         1 american     app 0.00
```
