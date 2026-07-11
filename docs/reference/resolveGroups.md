# Resolve Grouped Data

Standardizes grouped data supplied either as a numeric vector with
grouping variable or as a list of group-specific vectors.

## Usage

``` r
resolveGroups(x, groups)
```

## Arguments

- x:

  a numeric vector of observations, or a list of numeric vectors

- groups:

  a grouping variable of the same length as `x`. Ignored when `x` is a
  list.

## Value

A list containing:

- x:

  numeric response vector

- groups:

  grouping factor

- n:

  total number of observations

- k:

  number of groups

- group.sizes:

  group sample sizes

- group.names:

  group labels

- data.name:

  character description of the input

## Details

Missing observations are removed and grouping information is returned in
a consistent format suitable for hypothesis tests, summaries,
effect-size calculations and plotting functions.

## See also

Other data.resolve: [`resolveContingency()`](resolveContingency.md),
[`resolveFormula()`](resolveFormula.md)

## Examples

``` r
# vector + grouping variable
set.seed(1)
x <- rnorm(30)
g <- rep(c("a", "b", "c"), each = 10)
resolveGroups(x, g)
#> $x
#>  [1] -0.62645381  0.18364332 -0.83562861  1.59528080  0.32950777 -0.82046838
#>  [7]  0.48742905  0.73832471  0.57578135 -0.30538839  1.51178117  0.38984324
#> [13] -0.62124058 -2.21469989  1.12493092 -0.04493361 -0.01619026  0.94383621
#> [19]  0.82122120  0.59390132  0.91897737  0.78213630  0.07456498 -1.98935170
#> [25]  0.61982575 -0.05612874 -0.15579551 -1.47075238 -0.47815006  0.41794156
#> 
#> $groups
#>  [1] a a a a a a a a a a b b b b b b b b b b c c c c c c c c c c
#> Levels: a b c
#> 
#> $n
#> [1] 30
#> 
#> $k
#> [1] 3
#> 
#> $group.sizes
#> groups
#>  a  b  c 
#> 10 10 10 
#> 
#> $group.names
#> [1] "a" "b" "c"
#> 
#> $data.name
#> [1] "x and g"
#> 

# list of group-specific vectors
resolveGroups(list(a = rnorm(10), b = rnorm(12), c = rnorm(8)))
#> $x
#>  [1]  1.35867955 -0.10278773  0.38767161 -0.05380504 -1.37705956 -0.41499456
#>  [7] -0.39428995 -0.05931340  1.10002537  0.76317575 -0.16452360 -0.25336168
#> [13]  0.69696338  0.55666320 -0.68875569 -0.70749516  0.36458196  0.76853292
#> [19] -0.11234621  0.88110773  0.39810588 -0.61202639  0.34111969 -1.12936310
#> [25]  1.43302370  1.98039990 -0.36722148 -1.04413463  0.56971963 -0.13505460
#> 
#> $groups
#>  [1] a a a a a a a a a a b b b b b b b b b b b b c c c c c c c c
#> Levels: a b c
#> 
#> $n
#> [1] 30
#> 
#> $k
#> [1] 3
#> 
#> $group.sizes
#>  a  b  c 
#> 10 12  8 
#> 
#> $group.names
#> [1] "a" "b" "c"
#> 
#> $data.name
#> [1] "list(a = rnorm(10), b = rnorm(12), c = rnorm(8))"
#> 
```
