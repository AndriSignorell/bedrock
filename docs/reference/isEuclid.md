# Test if a Distance Matrix Is Euclidean

Checks whether a distance matrix corresponds to Euclidean distances.

## Usage

``` r
isEuclid(distmat, tol = 0.0000001)
```

## Arguments

- distmat:

  an object of class `dist`

- tol:

  numeric tolerance for detecting negative eigenvalues, relative to the
  largest absolute eigenvalue

## Value

A logical scalar. Returns `TRUE` if the distance matrix is
(approximately) Euclidean, otherwise `FALSE`.

## Details

The test is based on the eigenvalues of the double-centered squared
distance matrix \\B = -\frac{1}{2} J D^2 J\\. A distance matrix is
Euclidean if and only if \\B\\ is positive semi-definite, i.e., all
eigenvalues are non-negative (within numerical tolerance).

The tolerance is applied *relative* to the largest absolute eigenvalue,
so that the test is invariant to rescaling of the distances.

The returned logical value carries additional diagnostic information as
attributes:

- `eigenvalues`: Eigenvalues of the centered matrix

- `minEigenvalue`: Smallest eigenvalue

- `tol`: Tolerance used for the test

## See also

Other data.predicate: [`flags()`](flags.md),
[`isDichotomous()`](isDichotomous.md),
[`isLowCardinality()`](isLowCardinality.md),
[`isNumeric()`](isNumeric.md), [`isWholeLike()`](isWholeLike.md),
[`isZero()`](isZero.md), [`nUnique()`](nUnique.md)

## Examples

``` r
d <- dist(matrix(rnorm(20), ncol = 2))
res <- isEuclid(d)
res
#> [1] TRUE
#> attr(,"eigenvalues")
#>  [1]  1.851127e+01  6.672260e+00  2.664771e-15  1.230865e-15  6.547888e-16
#>  [6] -3.280886e-16 -4.537133e-16 -1.127479e-15 -1.651425e-15 -2.059317e-15
#> attr(,"minEigenvalue")
#> [1] -2.059317e-15
#> attr(,"tol")
#> [1] 1e-07

# Access diagnostics
attr(res, "eigenvalues")
#>  [1]  1.851127e+01  6.672260e+00  2.664771e-15  1.230865e-15  6.547888e-16
#>  [6] -3.280886e-16 -4.537133e-16 -1.127479e-15 -1.651425e-15 -2.059317e-15
attr(res, "minEigenvalue")
#> [1] -2.059317e-15
```
