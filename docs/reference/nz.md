# Extract Non-Zero Values

Returns all non-zero elements of a vector. Zeroness is determined by
[`isZero`](https://andrisignorell.github.io/bedrock/reference/isZero.md),
i.e. within a numerical tolerance.

## Usage

``` r
nz(x, tol = sqrt(.Machine$double.eps))
```

## Arguments

- x:

  a numeric vector.

- tol:

  tolerance passed to
  [`isZero`](https://andrisignorell.github.io/bedrock/reference/isZero.md).

## Value

a vector containing only the non-zero elements of `x`.

## Details

`NA` elements are not considered zero and are retained in the result.

## See also

[`isZero`](https://andrisignorell.github.io/bedrock/reference/isZero.md)

Other vector.utils:
[`unwhich()`](https://andrisignorell.github.io/bedrock/reference/unwhich.md)

## Examples

``` r
nz(c(0, 1, 2, 0, 3))
#> [1] 1 2 3
nz(c(1e-20, 1, NA))
#> [1]  1 NA
```
