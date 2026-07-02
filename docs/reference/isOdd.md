# Test if numbers are odd

Checks whether elements of a numeric vector are odd integers.

## Usage

``` r
isOdd(x)
```

## Arguments

- x:

  A numeric vector.

## Value

A logical vector of the same length as `x`. Returns `TRUE` for odd
integers, `FALSE` for even integers, and `NA` for non-integer or
non-finite values.

## Details

The function first checks whether values are finite integers.
Non-integer values (e.g. 3.5), `NA`, `NaN`, or `Inf` return `NA`.

## See also

Other number.theory: [`digitSum()`](digitSum.md),
[`divisors()`](divisors.md), [`factorize()`](factorize.md),
[`fibonacci()`](fibonacci.md), [`gcd_lcm`](gcd_lcm.md),
[`isPrime()`](isPrime.md),
[`numeric-conversions`](numeric-conversions.md), [`primes()`](primes.md)

## Examples

``` r
isOdd(1:5)
#> [1]  TRUE FALSE  TRUE FALSE  TRUE
isOdd(c(2, 3, 4.5, NA, Inf))
#> [1] FALSE  TRUE    NA    NA    NA
```
