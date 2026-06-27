# Count Unique Values

Returns the number of unique elements in a vector.

## Usage

``` r
nUnique(x, na.rm = FALSE)
```

## Arguments

- x:

  A vector.

- na.rm:

  Logical. Should missing values (`NA`) be removed before counting
  unique values? Defaults to `FALSE`.

## Value

An integer of length one.

## See also

[`isLowCardinality()`](isLowCardinality.md) to check whether `x` has
fewer than a given number of unique values, without counting all of them
first.

## Examples

``` r
nUnique(c(1, 1, 2, 3))
#> [1] 3

nUnique(c(1, 1, 2, NA))
#> [1] 3

nUnique(c(1, 1, 2, NA), na.rm = TRUE)
#> [1] 2
```
