# Replace Multiple Values in a Vector

Replaces elements of a character vector based on a lookup defined by two
parallel vectors. Each element exactly matching a pattern is replaced
with the corresponding replacement.

## Usage

``` r
mReplace(x, patterns, replacements)
```

## Arguments

- x:

  a character vector whose elements are to be replaced.

- patterns:

  a character vector of values to search for.

- replacements:

  a character vector of replacement values, in the same order as
  `patterns`.

## Value

a character vector of the same length as `x`, with matching elements
replaced. Non-matching elements are returned unchanged.

## See also

[`mGsub`](https://andrisignorell.github.io/bedrock/reference/mGsub.md)
for substring replacement.

Other data.recode:
[`asBinary()`](https://andrisignorell.github.io/bedrock/reference/asBinary.md),
[`combLevels()`](https://andrisignorell.github.io/bedrock/reference/combLevels.md),
[`dummy()`](https://andrisignorell.github.io/bedrock/reference/dummy.md),
[`nf()`](https://andrisignorell.github.io/bedrock/reference/nf.md),
[`recodeX()`](https://andrisignorell.github.io/bedrock/reference/recodeX.md),
[`revCode()`](https://andrisignorell.github.io/bedrock/reference/revCode.md),
[`stringsAsFactors()`](https://andrisignorell.github.io/bedrock/reference/stringsAsFactors.md)

## Examples

``` r
mReplace(c("a", "b", "c", "d"), c("a", "c"), c("A", "C"))
#> [1] "A" "b" "C" "d"
# [1] "A" "b" "C" "d"
```
