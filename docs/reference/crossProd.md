# Cross Product of 3D Vectors or Matrices

Computes the cross product in three-dimensional space for vectors or
matrices. For matrices, the operation can be applied row-wise or
column-wise.

## Usage

``` r
crossProd(x, y, orientation = c("rows", "cols"))
```

## Arguments

- x:

  a numeric or complex vector of length 3, or a matrix with one
  dimension of length 3.

- y:

  a numeric or complex vector or matrix with the same dimensions as `x`.

- orientation:

  character string specifying whether vectors are stored in rows or
  columns when matrices are supplied. Must be one of `"rows"` or
  `"cols"`. Ignored if `x` and `y` are vectors.

## Value

- a vector of length 3 if inputs are vectors.

- a matrix if matrices are supplied. Dimension names along the vector
  axis are propagated from `x`, the component axis is labelled `x`, `y`,
  `z`.

## Details

For vectors \\x = (x_1, x_2, x_3)\\ and \\y = (y_1, y_2, y_3)\\, the
cross product is: \$\$ x \times y = (x_2 y_3 - x_3 y_2,\\ x_3 y_1 - x_1
y_3,\\ x_1 y_2 - x_2 y_1) \$\$

For matrix inputs:

- `orientation = "rows"`: each row is treated as a vector (requires
  `ncol(x) == 3`)

- `orientation = "cols"`: each column is treated as a vector (requires
  `nrow(x) == 3`)

Numeric and complex inputs can be mixed; standard R coercion rules
apply.

## See also

Other math.basic: [`closest()`](closest.md),
[`crossProdN()`](crossProdN.md), [`dotProd()`](dotProd.md),
[`roundTo()`](roundTo.md), [`unirootAll()`](unirootAll.md)

## Examples

``` r
# Vector case
crossProd(c(1,0,0), c(0,1,0))
#> [1] 0 0 1

# Row-wise
x <- matrix(c(1,0,0,
              0,1,0), ncol = 3, byrow = TRUE)
y <- matrix(c(0,1,0,
              0,0,1), ncol = 3, byrow = TRUE)
crossProd(x, y, "rows")
#>      x y z
#> [1,] 0 0 1
#> [2,] 1 0 0

# Column-wise
x <- matrix(1:9, nrow = 3)
y <- matrix(9:1, nrow = 3)
crossProd(x, y, "cols")
#>   [,1] [,2] [,3]
#> x  -10  -10  -10
#> y   20   20   20
#> z  -10  -10  -10
```
