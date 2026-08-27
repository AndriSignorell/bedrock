# Dot Product of Vectors or Matrices

Computes the dot product between two numeric or complex vectors, or the
column-wise dot products of two matrices with identical dimensions.

## Usage

``` r
dotProd(x, y)
```

## Arguments

- x:

  a numeric or complex vector, or a numeric/complex matrix.

- y:

  a numeric or complex vector, or a numeric/complex matrix with the same
  dimensions as `x`.

## Value

- a scalar if `x` and `y` are vectors.

- a numeric or complex vector containing the column-wise dot products if
  matrices are supplied.

## Details

For vectors \\x\\ and \\y\\, the dot product is defined as: \$\$ \sum_i
\overline{x_i} y_i \$\$ where \\\overline{x_i}\\ denotes the complex
conjugate of \\x_i\\ (for real input this is simply \\\sum_i x_i y_i\\).

For matrices, the dot product of each column of `x` with the
corresponding column of `y` is returned.

Note that [`crossprod`](https://rdrr.io/r/base/crossprod.html) does
*not* conjugate its first argument for complex input, so it computes
\\t(X) Y\\ rather than the Hermitian inner product; this function does
conjugate.

## See also

[`crossprod`](https://rdrr.io/r/base/crossprod.html)

Other math.basic: [`closest()`](closest.md),
[`crossProd()`](crossProd.md), [`crossProdN()`](crossProdN.md),
[`roundTo()`](roundTo.md), [`unirootAll()`](unirootAll.md)

## Examples

``` r
# Vector dot product
dotProd(c(1, 2, 3), c(4, 5, 6))
#> [1] 32

# Complex vectors (Hermitian inner product)
dotProd(c(1+1i, 2), c(3, 4-1i))
#> [1] 11-5i

# Matrix (column-wise dot products)
x <- matrix(1:6, ncol = 2)
y <- matrix(6:1, ncol = 2)
dotProd(x, y)
#> [1] 28 28
```
