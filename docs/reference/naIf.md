# Replace Values with NA

Replaces specified values in a vector with `NA`, in the manner of SQL's
`NULLIF`. This is the complementary operation to
[`coalesceX`](coalesceX.md).

## Usage

``` r
naIf(x, values)
```

## Arguments

- x:

  a vector

- values:

  values to be replaced by `NA`

## Value

A vector of the same type as `x`.

## See also

Other vector.na: [`coalesceX()`](coalesceX.md), [`isNA()`](isNA.md),
[`locf()`](locf.md), [`naReplace()`](naReplace.md)

## Examples

``` r
naIf(c(1, 2, 99, 3, 99), 99)
#> [1]  1  2 NA  3 NA
naIf(c("a", "b", "n/a", ""), c("n/a", ""))
#> [1] "a" "b" NA  NA 
```
