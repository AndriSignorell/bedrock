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

  A character vector in which substitutions are performed.

- patterns:

  A character vector of substrings to search for (`fixed = TRUE`).

- replacements:

  A character vector of replacement strings, in the same order as
  `patterns`.

## Value

A character vector of the same length as `x`.

## See also

[`mReplace`](mReplace.md) for exact whole-element replacement.

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
