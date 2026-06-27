# Check a Vector For Being Zero

Test if x is zero. This is done by checking if the numeric value is
below the machine tolerance.

## Usage

``` r
isZero(x, tol = sqrt(.Machine$double.eps), na.rm = FALSE)
```

## Arguments

- x:

  a (non-empty) numeric vector of data values.

- tol:

  tolerance to be used

- na.rm:

  logical, indicating whether `NA` values should be stripped before the
  computation proceeds. Defaults to `FALSE`.

## Value

logical vector of the same dimension as x.

## See also

[`is.integer`](https://rdrr.io/r/base/integer.html)

Other data.inspection: [`allDuplicated()`](allDuplicated.md),
[`allIdentical()`](allIdentical.md),
[`completeColumns()`](completeColumns.md),
[`countCompCases()`](countCompCases.md), [`flags()`](flags.md),
[`isDichotomous()`](isDichotomous.md), [`isNumeric()`](isNumeric.md),
[`isWholeLike()`](isWholeLike.md)

## Examples

``` r
# ... These are people who live in ignorance of the Floating Point Gods.
# These pagans expect ... (Burns, 2011)" the following to be TRUE:
(.1 - .3 / 3) == 0
#> [1] FALSE

# they might be helped by
isZero(.1 - .3 / 3)
#> [1] TRUE
```
