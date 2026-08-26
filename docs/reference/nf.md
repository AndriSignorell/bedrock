# Convert to Numeric via Factor

Converts an object to numeric by first coercing it to a factor and then
to numeric. This is useful whenever a categorical or character variable
needs a purely numeric stand-in – for example, as input to functions
that require numeric data (distance calculations, correlation matrices,
some modelling routines), or to obtain a compact, deterministic
small-integer code for an ordinal variable by passing an explicit
`levels` order.

## Usage

``` r
nf(x, ...)
```

## Arguments

- x:

  a vector to be converted.

- ...:

  additional arguments passed to
  [`factor`](https://rdrr.io/r/base/factor.html).

## Value

a numeric vector corresponding to the integer codes of the factor
levels.

## Details

This function is a shorthand for `as.numeric(factor(x, ...))`.

Note that the resulting numeric values correspond to the internal factor
levels, not the original numeric values. In particular, for character
vectors holding numbers the codes follow the alphabetical level order
(see the last example) – use `as.numeric(as.character(x))` to recover
the values themselves.

## See also

[`factor`](https://rdrr.io/r/base/factor.html),
[`as.numeric`](https://rdrr.io/r/base/numeric.html)

Other data.recode:
[`asBinary()`](https://andrisignorell.github.io/bedrock/reference/asBinary.md),
[`combLevels()`](https://andrisignorell.github.io/bedrock/reference/combLevels.md),
[`dummy()`](https://andrisignorell.github.io/bedrock/reference/dummy.md),
[`mReplace()`](https://andrisignorell.github.io/bedrock/reference/mReplace.md),
[`recodeX()`](https://andrisignorell.github.io/bedrock/reference/recodeX.md),
[`revCode()`](https://andrisignorell.github.io/bedrock/reference/revCode.md),
[`stringsAsFactors()`](https://andrisignorell.github.io/bedrock/reference/stringsAsFactors.md)

## Examples

``` r
nf(c("a", "b", "a"))
#> [1] 1 2 1
nf(c("low", "medium", "high"), levels = c("low", "medium", "high"))
#> [1] 1 2 3

# caution: codes, not values
nf(c("10", "2"))    # 1 2, not 10 2
#> [1] 1 2
```
