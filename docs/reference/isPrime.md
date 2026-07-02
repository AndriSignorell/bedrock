# Test Whether Numbers Are Prime

Determines whether integer values are prime numbers.

## Usage

``` r
isPrime(n)
```

## Arguments

- n:

  A numeric vector. Values must be finite integers in the unsigned
  64-bit integer range.

## Value

A logical vector indicating whether each element of `n` is a prime
number.

## Details

This function is vectorized and returns a logical vector of the same
length as the input.

Internally, this function uses a fast deterministic primality test for
64-bit integers.

Non-integer or non-finite values will result in `FALSE`.

## See also

Other number.theory: [`digitSum()`](digitSum.md),
[`divisors()`](divisors.md), [`factorize()`](factorize.md),
[`fibonacci()`](fibonacci.md), [`gcd_lcm`](gcd_lcm.md),
[`isOdd()`](isOdd.md), [`numeric-conversions`](numeric-conversions.md),
[`primes()`](primes.md)

## Examples

``` r
isPrime(2)
#> [1] TRUE
isPrime(1:10)
#>  [1] FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE
isPrime(c(17, 18, 19))
#> [1]  TRUE FALSE  TRUE

```
