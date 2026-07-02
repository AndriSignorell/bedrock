# Replace Multiple Values in a Vector

Replaces elements of a character vector based on a named lookup table.
Each element matching a pattern is replaced with the corresponding
replacement.

## Usage

``` r
mReplace(x, patterns, replacements)
```

## Arguments

- x:

  A character vector whose elements are to be replaced.

- patterns:

  A character vector of values to search for.

- replacements:

  A character vector of replacement values, in the same order as
  `patterns`.

## Value

A character vector of the same length as `x`, with matching elements
replaced. Non-matching elements are returned unchanged.

## See also

Other string.utilities:
[`char-ascii-conversion`](char-ascii-conversion.md),
[`mGsub()`](mGsub.md), [`strSplitToCol()`](strSplitToCol.md),
[`strSplitToDummy()`](strSplitToDummy.md)

## Examples

``` r
mReplace(c("a", "b", "c", "d"), c("a", "c"), c("A", "C"))
#> [1] "A" "b" "C" "d"
# [1] "A" "b" "C" "d"
```
