# Inverse Which

Reconstructs the `TRUE` positions from the index vector returned by
[`which`](https://rdrr.io/r/base/which.html), producing a logical vector
of length `n`. Note that this is not a perfect inverse:
[`which()`](https://rdrr.io/r/base/which.html) discards `NA` and `FALSE`
positions, so the original vector cannot be fully recovered.

## Usage

``` r
unwhich(
  idx,
  n = if (length(idx) > 0L && !anyNA(idx) && all(idx > 0L)) max(idx) else 0L,
  useNames = TRUE
)
```

## Arguments

- idx:

  a vector of non-zero whole-number indices. Positive values mark `TRUE`
  positions; negative values mark `FALSE` positions (all others become
  `TRUE`). As in base R, positive and negative indices must not be
  mixed. Duplicate indices are allowed and result in a single `TRUE` (or
  `FALSE`) at that position.

- n:

  a single non-negative whole number giving the length of the result.
  For positive `idx`, defaults to `max(idx)`; for negative or empty
  `idx`, defaults to `0L`. Must not be less than `max(abs(idx))`.

- useNames:

  logical. If `TRUE` (default) *and* `idx` has names, those names are
  attached to the corresponding `TRUE` positions of the result; all
  other positions receive an empty string. If `FALSE` or `idx` is
  unnamed, the result has no names. Ignored for negative indices.

## Value

A logical vector of length `n`.

## Details

Negative indices follow standard R semantics: `unwhich(-2, 5)` returns a
vector with `TRUE` everywhere *except* position 2. Positive and negative
indices must not be mixed.

## Note

The positive-index construction (`rv[indices] <- TRUE` with name
propagation) is based on code by Nick Sabbe; negative-index handling and
input validation are original additions.

## References

Sabbe, N. (2012). Inverse of `which`.
<https://stackoverflow.com/questions/7659833/inverse-of-which>

## See also

[`which`](https://rdrr.io/r/base/which.html)

Other vector.utils: [`nz()`](nz.md)

## Examples

``` r
ll <- c(TRUE, FALSE, TRUE, NA, FALSE, FALSE, TRUE)
names(ll) <- letters[seq_along(ll)]
i <- which(ll)

# reconstruct TRUE positions (names preserved on TRUE positions)
unwhich(i, length(ll))
#>     a           c                       g 
#>  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE 

# without names
unwhich(i, length(ll), useNames = FALSE)
#> [1]  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE

# negative index: TRUE everywhere except position 2
unwhich(-2, 5)
#> [1]  TRUE FALSE  TRUE  TRUE  TRUE

# empty index -> all-FALSE vector
unwhich(integer(0), n = 5L)
#> [1] FALSE FALSE FALSE FALSE FALSE
```
