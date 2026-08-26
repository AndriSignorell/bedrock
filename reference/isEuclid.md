# Test if a Distance Matrix Is Euclidean

Checks whether a distance matrix corresponds to Euclidean distances.

## Usage

``` r
isEuclid(distmat, tol = 0.0000001)
```

## Arguments

- distmat:

  an object of class `dist`.

- tol:

  numeric tolerance for detecting negative eigenvalues, relative to the
  largest absolute eigenvalue.

## Value

a logical scalar. Returns `TRUE` if the distance matrix is
(approximately) Euclidean, otherwise `FALSE`.

## Details

The test is based on the eigenvalues of the double-centered squared
distance matrix \\B = -\frac{1}{2} J D^2 J\\. A distance matrix is
Euclidean if and only if \\B\\ is positive semi-definite, i.e., all
eigenvalues are non-negative (within numerical tolerance).

The tolerance is applied *relative* to the largest absolute eigenvalue,
so that the test is invariant to rescaling of the distances. Note that
this holds in both directions: the comparison below uses
`max(abs(lambda))` without an absolute floor, so shrinking all distances
by a constant factor cannot turn a non-Euclidean matrix into a Euclidean
one.

The returned logical value carries additional diagnostic information as
attributes:

- `eigenvalues`: Eigenvalues of the centered matrix

- `minEigenvalue`: Smallest eigenvalue

- `tol`: Tolerance used for the test

## See also

Other data.predicate:
[`flags()`](https://andrisignorell.github.io/bedrock/reference/flags.md),
[`isDichotomous()`](https://andrisignorell.github.io/bedrock/reference/isDichotomous.md),
[`isLowCardinality()`](https://andrisignorell.github.io/bedrock/reference/isLowCardinality.md),
[`isNumeric()`](https://andrisignorell.github.io/bedrock/reference/isNumeric.md),
[`isWholeLike()`](https://andrisignorell.github.io/bedrock/reference/isWholeLike.md),
[`isZero()`](https://andrisignorell.github.io/bedrock/reference/isZero.md),
[`nUnique()`](https://andrisignorell.github.io/bedrock/reference/nunique.md)

## Examples

``` r
d <- dist(matrix(rnorm(20), ncol = 2))
res <- isEuclid(d)
res
#> [1] TRUE
#> attr(,"eigenvalues")
#>  [1]  1.851127e+01  6.672260e+00  2.515731e-15  1.085907e-15  8.331918e-16
#>  [6]  4.815763e-16 -1.135402e-16 -1.545953e-15 -1.889299e-15 -3.017920e-15
#> attr(,"minEigenvalue")
#> [1] -3.01792e-15
#> attr(,"tol")
#> [1] 1e-07

# Access diagnostics
attr(res, "eigenvalues")
#>  [1]  1.851127e+01  6.672260e+00  2.515731e-15  1.085907e-15  8.331918e-16
#>  [6]  4.815763e-16 -1.135402e-16 -1.545953e-15 -1.889299e-15 -3.017920e-15
attr(res, "minEigenvalue")
#> [1] -3.01792e-15
```
