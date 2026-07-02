# Generalized Cross Product via Determinants

Computes a vector orthogonal to all rows of a matrix using a
determinant-based construction. This generalizes the cross product to
higher dimensions.

## Usage

``` r
crossProdN(A)
```

## Arguments

- A:

  A numeric or complex vector of length 2, or a matrix of dimension \\n
  \times (n+1)\\.

## Value

A numeric or complex vector of length \\n+1\\.

## Details

For a matrix \\A\\ with dimensions \\n \times (n+1)\\, the result is a
vector in \\\mathbb{R}^{n+1}\\ orthogonal to all rows of \\A\\. The
components are given by: \$\$ v_i = (-1)^{i+1} \det(A\_{-i}) \$\$ where
\\A\_{-i}\\ is the matrix obtained by removing the \\i\\-th column.

For a vector of length 2, the function returns a perpendicular vector.

This function computes a nullspace vector using SVD and rescales it to
match the magnitude of the determinant-based generalized cross product.
The sign is fixed by enforcing the first non-zero component to be
positive.

## See also

Other math.utils: [`crossProd()`](crossProd.md),
[`dotProd()`](dotProd.md), [`linScale()`](linScale.md),
[`logit()`](logit.md), [`nUnique()`](nUnique.md),
[`percentRank()`](percentRank.md), [`precision`](precision.md),
[`rankX()`](rankX.md), [`roundTo()`](roundTo.md),
[`unirootAll()`](unirootAll.md), [`untable()`](untable.md)

## Examples

``` r
# 2D case
crossProdN(c(1, 2))
#> [1] -2  1

# 3D case (standard cross product)
A <- matrix(c(1,0,0,
              0,1,0), nrow = 2, byrow = TRUE)
crossProdN(A)
#> [1] 0 0 1
```
