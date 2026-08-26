# Test Whether Numbers Are Prime

Determines whether integer values are prime numbers.

## Usage

``` r
isPrime(n)
```

## Arguments

- n:

  a numeric vector. Values must be finite integers in the unsigned
  64-bit integer range.

## Value

a logical vector indicating whether each element of `n` is a prime
number.

## Details

This function is vectorized and returns a logical vector of the same
length as the input.

Internally, a fast deterministic primality test for 64-bit integers is
used.

Non-integer, negative, missing, or non-finite values result in `FALSE`.

## See also

Other number.theory:
[`GCD-LCM`](https://andrisignorell.github.io/bedrock/reference/GCD-LCM.md),
[`digitSum()`](https://andrisignorell.github.io/bedrock/reference/digitSum.md),
[`divisors()`](https://andrisignorell.github.io/bedrock/reference/divisors.md),
[`factorize()`](https://andrisignorell.github.io/bedrock/reference/factorize.md),
[`fibonacci()`](https://andrisignorell.github.io/bedrock/reference/fibonacci.md),
[`isOdd()`](https://andrisignorell.github.io/bedrock/reference/isOdd.md),
[`primes()`](https://andrisignorell.github.io/bedrock/reference/primes.md)

## Examples

``` r
isPrime(2)
#> [1] TRUE
isPrime(1:10)
#>  [1] FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE
isPrime(c(17, 18, 19))
#> [1]  TRUE FALSE  TRUE
```
