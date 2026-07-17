# Randomly Split a Vector into Groups of Given Sizes

Randomly assigns the elements of a vector `x` into groups with
predefined sizes given by `groupSizes`. The grouping is performed
without replacement and each element is assigned to exactly one group.

## Usage

``` r
randGroupSplit(x, groupSizes)
```

## Arguments

- x:

  a vector containing the elements to be split into groups.

- groupSizes:

  an integer vector specifying the sizes of the groups. The sum of
  `groupSizes` must equal `length(x)`. If the vector is named, the names
  are used as group names in the result.

## Value

a list of vectors, where each element corresponds to one group. The
length of the list equals `length(groupSizes)`.

## Details

This function is useful for random group assignments, for example in
teaching settings, simulations, or experimental designs where groups of
unequal sizes are required. It uses
[`sample`](https://rdrr.io/r/base/sample.html), so results can be made
reproducible with [`set.seed`](https://rdrr.io/r/base/Random.html).

## See also

Other combinatorics: [`combN()`](combN.md),
[`combPairs()`](combPairs.md), [`combSet()`](combSet.md),
[`pairApply()`](pairApply.md), [`permn()`](permn.md),
[`sampleX()`](sampleX.md)

## Examples

``` r
# Split letters into 3 groups of sizes 4, 3, and 5
set.seed(123)
randGroupSplit(LETTERS[1:12], groupSizes = c(4, 3, 5))
#> $`1`
#> [1] "A" "D" "H" "K"
#> 
#> $`2`
#> [1] "E" "G" "L"
#> 
#> $`3`
#> [1] "B" "C" "F" "I" "J"
#> 

# named groups
randGroupSplit(LETTERS[1:7], groupSizes = c(treat = 4, ctrl = 3))
#> $treat
#> [1] "A" "E" "F" "G"
#> 
#> $ctrl
#> [1] "B" "C" "D"
#> 
```
