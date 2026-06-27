# Generate Prime Numbers up to Given Limits

Computes all prime numbers less than or equal to each value in `n`.

## Usage

``` r
primes(n)
```

## Arguments

- n:

  A numeric vector of positive integers.

## Value

A named list. Each element is an integer vector containing the prime
numbers less than or equal to the corresponding value in `n`.

## Details

The function is vectorized over `n` and returns a named list, where each
element contains the prime numbers up to the corresponding value.

For each element of `n`, the primes are calculated. The result is
returned as a named list, with names corresponding to the input values.

## See also

Other number.theory: [`digitSum()`](digitSum.md),
[`factorize()`](factorize.md), [`fibonacci()`](fibonacci.md),
[`isOdd()`](isOdd.md), [`isPrime()`](isPrime.md),
[`numeric-conversions`](numeric-conversions.md)

## Examples

``` r
primes(10)
#> $`10`
#> [1] 2 3 5 7
#> 
primes(c(5, 10))
#> $`5`
#> [1] 2 3 5
#> 
#> $`10`
#> [1] 2 3 5 7
#> 
```
