# Normalize and recycle interval inputs

Internal helper to ensure intervals are ordered (min, max) and recycled
to equal length.

## Usage

``` r
.intervalEngine(x, y)
```

## Arguments

- x:

  Numeric vector or matrix with 2 columns

- y:

  Numeric vector or matrix with 2 columns

## Value

A list with normalized and recycled matrices `x` and `y`
