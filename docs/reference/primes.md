# Generate Prime Numbers up to Given Limits

Computes all prime numbers less than or equal to each value in `n`.

## Usage

``` r
primes(n)
```

## Arguments

- n:

  a numeric vector of positive whole numbers

## Value

An integer vector containing the prime numbers less than or equal to `n`
if `n` is a single number, otherwise a named list of such vectors.

## Details

The function is vectorized over `n`. For a single value, the primes are
returned as an integer vector; for several values, a named list is
returned, with names corresponding to the input values.

## See also

Other number.theory: [`GCD-LCM`](GCD-LCM.md),
[`digitSum()`](digitSum.md), [`divisors()`](divisors.md),
[`factorize()`](factorize.md), [`fibonacci()`](fibonacci.md),
[`isOdd()`](isOdd.md), [`isPrime()`](isPrime.md)

## Examples

``` r
primes(10)
#> [1] 2 3 5 7
primes(c(5, 10))
#> $`5`
#> [1] 2 3 5
#> 
#> $`10`
#> [1] 2 3 5 7
#> 
```
