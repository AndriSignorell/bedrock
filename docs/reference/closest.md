# Find the Closest Value

Find the value(s) in a vector closest to a reference value. Multiple
values are returned if ties occur or if duplicate values share the same
minimum distance.

## Usage

``` r
closest(x, a, output = "value", na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector to search in.

- a:

  The reference value. May be a vector; see Details.

- output:

  Character string specifying the output representation.

  One of:

  `\"value\"`

  :   Return the closest value(s).

  `\"index\"`

  :   Return the index position(s) in `x`.

  Default is `\"value\"`.

  May be a vector; recycled to the length of `a`.

- na.rm:

  Logical.

  If `TRUE`, `NA` values in `x` are ignored before searching.

  Default is `FALSE`.

## Value

If `a` and `output` are scalar:

- numeric vector if `output = \"value\"`

- integer vector if `output = \"index\"`

If `a` or `output` are vectors: a list with one element per value of
`a`.

Returns `NA` if `x` is empty or all-`NA` (with `na.rm = TRUE`).

## Details

Distance is computed as \\\|x_i - a\|\\. Ties are detected via
[`isZero()`](isZero.md) rather than exact equality, which avoids
spurious misses due to floating-point representation (e.g.
`0.3 - 0.2 != 0.1`).

When `na.rm = TRUE`, `NA` elements are excluded from the search but the
original index positions of the remaining elements are preserved, so
`output = "index"` always refers to positions in the original `x`.

When `a` or `output` are vectors, each element is processed
independently and a list is returned.

Recycling follows standard R rules.

## See also

[`which`](https://rdrr.io/r/base/which.html)

Other vector.ops: [`coalesceX()`](coalesceX.md), [`locf()`](locf.md),
[`midx()`](midx.md), [`moveAvg()`](moveAvg.md), [`naIf()`](naIf.md),
[`naReplace()`](naReplace.md), [`nz()`](nz.md),
[`pairApply()`](pairApply.md), [`setLength()`](setLength.md),
[`trim()`](trim.md), [`vRot()`](vRot.md), [`vShift()`](vShift.md),
[`winsorize()`](winsorize.md)

## Examples

``` r
# basic
set.seed(8)
x <- runif(10) * 10

closest(x, 3.1)
#> [1] 3.215092

sort(x)
#>  [1] 2.078233 2.908734 3.215092 4.662952 6.444911 6.518713 7.189275 7.691470
#>  [9] 7.996580 9.322698

y <- sample(10, size = 10, replace = TRUE)

# multiple observations of the same closest value
closest(y, a = 6)
#> [1] 6 6

# get the relevant positions
closest(y, a = 6, output = "index")
#> [1] 5 6

# two different values having the same distance (tie)
closest(c(2, 3, 4, 5), a = 3.5)
#> [1] 3 4

# na.rm preserves original index positions
closest(
  c(NA, 5, 8),
  a = 6,
  output = "index",
  na.rm = TRUE
)  # 2, not 1
#> [1] 2

# vectorize "a"
closest(c(2, 3, 4, 5), a = c(3.1, 3.9))
#> [[1]]
#> [1] 3
#> 
#> [[2]]
#> [1] 4
#> 

# vectorize "output"
closest(
  c(2, 3, 4, 5),
  a = 3.1,
  output = c("value", "index")
)
#> [[1]]
#> [1] 3
#> 
#> [[2]]
#> [1] 2
#> 

closest(
  c(2, 3, 4, 5),
  a = c(3.1, 3.9),
  output = c("value", "index")
)
#> [[1]]
#> [1] 3
#> 
#> [[2]]
#> [1] 3
#> 
```
