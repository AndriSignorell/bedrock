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

  A vector containing the elements to be split into groups.

- groupSizes:

  An integer vector specifying the sizes of the groups. The sum of
  `groupSizes` must equal `length(x)`.

## Value

A list of vectors, where each element corresponds to one group. The
length of the list equals `length(groupSizes)`.

## Details

This function is useful for random group assignments, for example in
teaching settings, simulations, or experimental designs where groups of
unequal sizes are required.

## See also

Other combinatorics: [`combN()`](combN.md),
[`combPairs()`](combPairs.md), [`combSet()`](combinatoric.md),
[`permn()`](permn.md), [`sampleX()`](sampleX.md)

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
```
