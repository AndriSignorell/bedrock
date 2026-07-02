# Dot Product of Vectors or Matrices

Computes the dot product between two numeric or complex vectors or
matrices. Internally uses
[`crossprod`](https://rdrr.io/r/base/crossprod.html) for efficient
computation.

## Usage

``` r
dotProd(x, y)
```

## Arguments

- x:

  A numeric or complex vector, or a numeric/complex matrix.

- y:

  A numeric or complex vector, or a numeric/complex matrix with the same
  dimensions as `x`.

## Value

- A scalar if `x` and `y` are vectors.

- A numeric or complex vector containing column-wise dotProd products if
  matrices are supplied.

## Details

For vectors \\x\\ and \\y\\, the dot product is defined as: \$\$ \sum_i
\overline{x_i} y_i \$\$ where \\\overline{x_i}\\ denotes the complex
conjugate of \\x_i\\.

For matrices, the dot product is computed column-wise, corresponding to:
\$\$ X^H Y \$\$ where \\X^H\\ is the conjugate transpose of \\X\\.

The implementation relies on
[`crossprod`](https://rdrr.io/r/base/crossprod.html), which is typically
optimized via BLAS for high performance.

## See also

[`crossprod`](https://rdrr.io/r/base/crossprod.html)

Other math.utils: [`crossProd()`](crossProd.md),
[`crossProdN()`](crossProdN.md), [`linScale()`](linScale.md),
[`logit()`](logit.md), [`nUnique()`](nUnique.md),
[`percentRank()`](percentRank.md), [`precision`](precision.md),
[`rankX()`](rankX.md), [`roundTo()`](roundTo.md),
[`unirootAll()`](unirootAll.md), [`untable()`](untable.md)

## Examples

``` r
# Vector dot product
dotProd(c(1, 2, 3), c(4, 5, 6))
#> [1] 32

# Complex vectors
dotProd(c(1+1i, 2), c(3, 4-1i))
#> [1] 11+1i

# Matrix (column-wise dot products)
x <- matrix(1:6, ncol = 2)
y <- matrix(6:1, ncol = 2)
dotProd(x, y)
#>      [,1] [,2]
#> [1,]   28   10
#> [2,]   73   28
```
