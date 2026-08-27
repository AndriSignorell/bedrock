# Multiple String Substitution

Replaces multiple substrings in a character vector simultaneously,
avoiding the cascade problem where an earlier replacement becomes the
target of a later one. Internally uses temporary unique tokens as an
intermediate step.

## Usage

``` r
mGsub(x, patterns, replacements)
```

## Arguments

- x:

  a character vector in which substitutions are performed.

- patterns:

  a character vector of substrings to search for (`fixed = TRUE`).

- replacements:

  a character vector of replacement strings, in the same order as
  `patterns`.

## Value

a character vector of the same length as `x`.

## Details

Patterns are processed in the given order. For overlapping patterns
(e.g. `"AB"` and `"A"`), list the longer pattern first, otherwise the
shorter one consumes its characters before the longer one is considered.

## See also

[`mReplace`](mReplace.md) for exact whole-element replacement.

Other string.transform: [`strSplitToCol()`](strSplitToCol.md),
[`strSplitToDummy()`](strSplitToDummy.md)

## Examples

``` r
mGsub(c("foo bar", "bar foo"), c("foo", "bar"), c("bar", "foo"))
#> [1] "bar foo" "foo bar"
# [1] "bar foo" "foo bar"

# Without simultaneous replacement this would yield "foo foo"
# with sequential gsub().

x <- c("A", "B", "AB", "BA")
mGsub(x, patterns = c("A", "B"), replacements = c("BX", "CY"))
#> [1] "BX"   "CY"   "BXCY" "CYBX"
```
