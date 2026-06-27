# Resolve Grouped Data

Standardizes grouped data supplied either as a numeric vector with
grouping variable or as a list of group-specific vectors.

## Usage

``` r
resolveGroups(x, g)
```

## Arguments

- x:

  a numeric vector of observations, or a list of numeric vectors.

- g:

  a grouping variable of the same length as `x`. Ignored when `x` is a
  list.

## Value

A list containing:

- x:

  numeric response vector

- g:

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

Other data.utils: [`resolveContingency()`](resolveContingency.md)
