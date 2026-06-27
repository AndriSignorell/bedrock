# Test for a Scalar Missing Value

Check whether an object is a single missing value (`NA`).

## Usage

``` r
isNA(x)
```

## Arguments

- x:

  An object to be tested.

## Value

Logical scalar. Returns `TRUE` if `x` is a single missing value (`NA`),
and `FALSE` otherwise.

## Details

This is a strict helper that returns `TRUE` only if `x` is an atomic
vector of length one and equal to `NA`. In contrast to
[`is.na`](https://rdrr.io/r/base/NA.html), which is vectorized, `isNA`
is intended for scalar checks, e.g. in conditional statements.

This function differs from [`is.na`](https://rdrr.io/r/base/NA.html) in
that it:

- Only returns `TRUE` for length-one inputs

- Returns a single logical value (not vectorized)

- Works consistently across all NA types

## See also

Other utilities: [`callIf()`](callIf.md)

## Examples

``` r
isNA(NA)
#> [1] TRUE
isNA(NA_real_)
#> [1] TRUE
isNA(NA_integer_)
#> [1] TRUE

isNA(c(NA, NA))      # FALSE (length > 1)
#> [1] FALSE
isNA(NULL)           # FALSE
#> [1] FALSE
isNA(1)              # FALSE
#> [1] FALSE
isNA(c(1, NA))       # FALSE
#> [1] FALSE
```
