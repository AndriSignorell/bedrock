# Generate Prime Numbers up to Given Limits

Computes all prime numbers less than or equal to each value in `n`.

## Usage

``` r
primes(n)
```

## Arguments

- n:

  a numeric vector of positive whole numbers, none exceeding
  100,000,000.

## Value

an integer vector containing the prime numbers less than or equal to `n`
in ascending order if `n` is a single number, otherwise a named list of
such vectors.

## Details

The function is vectorized over `n`. For a single value, the primes are
returned as an integer vector; for several values, a named list is
returned, with names corresponding to the input values.

## Upper limit

`n` may not exceed 100,000,000. The limit is a practical one, not a
limit of the type: the sieve of Eratosthenes needs one bit per candidate
and one integer per prime found, which at 100 million comes to roughly
12.5 MB for the sieve, 23 MB for the 5,761,455 primes, and a peak below
about 70 MB once the copy into R is counted. At `.Machine$integer.max`
the same three figures are 268 MB, 105,097,565 primes for 420 MB, and a
peak beyond a gigabyte - which is why the integer limit is not a
sensible bound here. A substantially larger range would call for a
segmented sieve rather than for a larger allocation.

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

# the number of primes below a limit
length(primes(1e6))
#> [1] 78498
```
