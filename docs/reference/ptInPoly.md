# Point-in-Polygon Test (Angle Summation, Rcpp)

Determines whether points lie inside a polygon. Points located exactly
on polygon edges or vertices are treated as **inside**.

## Usage

``` r
ptInPoly(x, y, polyX, polyY)
```

## Arguments

- x:

  numeric vector of x-coordinates of the query points.

- y:

  numeric vector of y-coordinates of the query points.

- polyX:

  numeric vector of x-coordinates of the polygon vertices.

- polyY:

  numeric vector of y-coordinates of the polygon vertices.

## Value

an integer vector of length `length(x)`:

- 0:

  point is outside the polygon.

- 1:

  point is inside the polygon or on its boundary.

## Details

The function uses a numerically stable angle summation algorithm
implemented in C++ via Rcpp.

- The polygon is implicitly closed (last vertex connects to the first).

- The polygon is assumed to be non-self-intersecting.

- Numerical robustness is ensured via epsilon-based comparisons.

## Examples

``` r
# Define a square
px <- c(0, 1, 1, 0)
py <- c(0, 0, 1, 1)

# Query points
x <- c(0.5, 1.5, 0, 0.5)
y <- c(0.5, 0.5, 0, 1)

ptInPoly(x, y, px, py)
#> [1] 1 0 1 1
```
