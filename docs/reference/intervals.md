# Interval Arithmetic

Functions for computing relationships between numeric intervals. All
functions accept intervals as numeric vectors of length 2 or matrices
with 2 columns (one interval per row). Unordered bounds are silently
sorted; rows are recycled to equal length.

## Usage

``` r
overlap(x, y)

overlaps(x, y)

distance(x, y)

x %overlaps% y
```

## Arguments

- x:

  a numeric vector of length 2 `c(lower, upper)`, or a numeric matrix
  with 2 columns where each row defines one interval.

- y:

  a numeric vector of length 2 `c(lower, upper)`, or a numeric matrix
  with 2 columns where each row defines one interval.

## Value

- `overlap`:

  numeric vector of overlap lengths (0 if no overlap).

- `overlaps`:

  logical vector; `TRUE` if intervals share at least one point.

- `distance`:

  numeric vector of gap lengths between non-overlapping intervals (0 if
  overlapping or touching).

- `%overlaps%`:

  logical vector; operator wrapper for `overlaps()`.

## Details

Intervals are treated as closed, i.e., \\\[a, b\]\\. Consequently:

- Two intervals sharing only a boundary point have `overlap` 0 but
  `overlaps` returns `TRUE`.

- `distance` returns 0 whenever intervals touch or overlap.

## See also

Other data.interval: [`between-operators`](between-operators.md),
[`range-operators`](range-operators.md)

## Examples

``` r
# overlap length
overlap(c(1, 5), c(3, 7))   # 2
#> [1] 2
overlap(c(1, 3), c(3, 5))   # 0 (boundary only)
#> [1] 0

# overlap check
overlaps(c(1, 5), c(3, 7))  # TRUE
#> [1] TRUE
overlaps(c(1, 3), c(3, 5))  # TRUE  (boundary counts)
#> [1] TRUE
overlaps(c(1, 2), c(3, 4))  # FALSE
#> [1] FALSE

# gap distance
distance(c(1, 2), c(4, 5))  # 2
#> [1] 2
distance(c(1, 5), c(3, 7))  # 0
#> [1] 0

# operator
c(1, 5) %overlaps% c(3, 7)  # TRUE
#> [1] TRUE

# vectorised (matrix input)
m <- matrix(c(1,3, 2,6, 5,8), ncol = 2, byrow = TRUE)
overlap(m, c(4, 7))
#> [1] 0 2 2
```
