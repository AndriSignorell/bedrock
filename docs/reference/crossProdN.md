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

  a numeric or complex vector of length 2, or a matrix of dimension \\n
  x (n+1)\\.

## Value

a numeric or complex vector of length \\n+1\\.

## Details

For a matrix \\A\\ with dimensions \\n x (n+1)\\, the result is a vector
in \\R^{n+1}\\ orthogonal to all rows of \\A\\. The components are given
by: \$\$ v_i = (-1)^{i+1} \det(A\_{-i}) \$\$ where \\A\_{-i}\\ is the
matrix obtained by removing the \\i\\-th column.

For a vector of length 2, the function returns the perpendicular vector
\\(a_2, -a_1)\\, consistent with the formula above.

Internally, the function computes a nullspace vector using SVD (which is
numerically stable also for ill-conditioned input) and rescales it to
match the magnitude of the determinant-based generalized cross product.
For numeric input, the sign is chosen to reproduce the orientation of
the determinant formula (and hence anticommutativity: swapping two rows
of \\A\\ flips the sign of the result). For complex input, where
[`det()`](https://rdrr.io/r/base/det.html) is not available, the sign is
fixed by the convention that the first component with non-zero modulus
has a positive real part.

## See also

Other math.basic: [`closest()`](closest.md),
[`crossProd()`](crossProd.md), [`dotProd()`](dotProd.md),
[`roundTo()`](roundTo.md), [`unirootAll()`](unirootAll.md)

## Examples

``` r
# 2D case
crossProdN(c(1, 2))
#> [1]  2 -1

# 3D case (standard cross product)
A <- matrix(c(1,0,0,
              0,1,0), nrow = 2, byrow = TRUE)
crossProdN(A)
#> [1] 0 0 1

# swapping rows flips the sign (anticommutativity)
crossProdN(A[2:1, ])
#> [1]  0  0 -1
```
