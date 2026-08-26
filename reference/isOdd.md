# Test if Numbers Are Odd

Checks whether elements of a numeric vector are odd integers.

## Usage

``` r
isOdd(x)
```

## Arguments

- x:

  a numeric vector.

## Value

a logical vector of the same length as `x`. Returns `TRUE` for odd
integers, `FALSE` for even integers, and `NA` for non-integer or
non-finite values.

## Details

The function first checks whether values are finite integers.
Non-integer values (e.g. 3.5), `NA`, `NaN`, or `Inf` return `NA`. A bare
logical `NA` is accepted and treated as a missing numeric value.

## See also

Other number.theory:
[`GCD-LCM`](https://andrisignorell.github.io/bedrock/reference/GCD-LCM.md),
[`digitSum()`](https://andrisignorell.github.io/bedrock/reference/digitSum.md),
[`divisors()`](https://andrisignorell.github.io/bedrock/reference/divisors.md),
[`factorize()`](https://andrisignorell.github.io/bedrock/reference/factorize.md),
[`fibonacci()`](https://andrisignorell.github.io/bedrock/reference/fibonacci.md),
[`isPrime()`](https://andrisignorell.github.io/bedrock/reference/isPrime.md),
[`primes()`](https://andrisignorell.github.io/bedrock/reference/primes.md)

## Examples

``` r
isOdd(1:5)
#> [1]  TRUE FALSE  TRUE FALSE  TRUE
isOdd(c(2, 3, 4.5, NA, Inf))
#> [1] FALSE  TRUE    NA    NA    NA
```
