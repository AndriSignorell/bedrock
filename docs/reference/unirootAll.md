# Find multiple roots of a function within an interval

Searches a numeric interval for all roots (zeros) of a function `f` by
subdividing it into `n` sub-intervals, detecting sign changes, and
refining each candidate with
[`uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Usage

``` r
unirootAll(
  f,
  interval,
  lower = min(interval),
  upper = max(interval),
  tol = .Machine$double.eps^0.5,
  maxiter = 1000,
  n = 100,
  ...
)
```

## Arguments

- f:

  a function for which roots are sought. Must accept a numeric first
  argument; additional arguments are passed via `...`.

- interval:

  a numeric vector of length 2 specifying the search interval. Either
  `interval` or both `lower` and `upper` must be supplied.

- lower:

  lower bound of the search interval. Default: `min(interval)`.

- upper:

  upper bound of the search interval. Default: `max(interval)`.

- tol:

  convergence tolerance passed to
  [`uniroot`](https://rdrr.io/r/stats/uniroot.html), and also used as
  the threshold for (i) treating grid-point values as exact zeros
  and (ii) collapsing near-duplicate roots. Default:
  `.Machine$double.eps^0.5`.

- maxiter:

  maximum number of iterations for
  [`uniroot`](https://rdrr.io/r/stats/uniroot.html). Default: `1000`.

- n:

  number of sub-intervals used for the initial grid search. Increase `n`
  if roots may be close together or the function oscillates rapidly.
  Default: `100`.

- ...:

  additional arguments passed to `f`.

## Value

a numeric vector of roots found in `[lower, upper]`, sorted in ascending
order. Returns `numeric(0)` if no roots are found.

## Details

The function `f` is called as `f(x, ...)` where `x` is a numeric vector.
If `f` does not accept vector input, it is called element-wise via
`sapply`.

Grid points at which `|f(x)| < tol` are returned directly as roots. Sign
changes are detected using [`sign()`](https://rdrr.io/r/base/sign.html),
which avoids numerical overflow that can occur with product-based
approaches. Non-finite function values are silently ignored when
detecting sign changes. If `uniroot` fails on a sub-interval, that
interval is skipped with a warning rather than aborting the entire
search.

**Limitations:** Roots within the same sub-interval of width
`(upper - lower) / n` may be missed. Roots of even multiplicity that do
not produce a sign change will not be found unless they happen to fall
on a grid point. A warning is issued if no roots are found at all
despite finite function values being present.

## See also

[`uniroot`](https://rdrr.io/r/stats/uniroot.html) for the underlying
single-root solver.

Other math.basic: [`closest()`](closest.md),
[`crossProd()`](crossProd.md), [`crossProdN()`](crossProdN.md),
[`dotProd()`](dotProd.md), [`roundTo()`](roundTo.md)

## Examples

``` r
f <- function(x) cos(2 * x)^3
roots <- unirootAll(f, c(0, 10))
stopifnot(all(abs(f(roots)) < 1e-6))

# Non-vectorized function
g <- Vectorize(function(x) integrate(function(t) t^x, 0, 1)$value - 0.5)
unirootAll(g, c(0.1, 5))
#> [1] 1
```
