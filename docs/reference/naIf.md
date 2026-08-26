# Replace Values with NA

Replaces specified values in a vector with `NA`, in the manner of SQL's
`NULLIF`. This is the complementary operation to
[`coalesceX`](https://andrisignorell.github.io/bedrock/reference/coalesceX.md).

## Usage

``` r
naIf(x, values)
```

## Arguments

- x:

  a vector.

- values:

  values to be replaced by `NA`.

## Value

a vector of the same type as `x`.

## See also

Other vector.na:
[`coalesceX()`](https://andrisignorell.github.io/bedrock/reference/coalesceX.md),
[`isNA()`](https://andrisignorell.github.io/bedrock/reference/isNA.md),
[`locf()`](https://andrisignorell.github.io/bedrock/reference/locf.md),
[`naReplace()`](https://andrisignorell.github.io/bedrock/reference/naReplace.md)

## Examples

``` r
naIf(c(1, 2, 99, 3, 99), 99)
#> [1]  1  2 NA  3 NA
naIf(c("a", "b", "n/a", ""), c("n/a", ""))
#> [1] "a" "b" NA  NA 
```
