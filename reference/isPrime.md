# Test Whether Numbers Are Prime

Determines whether integer values are prime numbers.

## Usage

``` r
isPrime(n)
```

## Arguments

- n:

  a numeric vector. Values must be finite whole numbers not exceeding
  `2^53`.

## Value

a logical vector indicating whether each element of `n` is a prime
number, `NA` where `n` exceeds `2^53`.

## Details

This function is vectorized and returns a logical vector of the same
length as the input.

Internally, a fast deterministic primality test for 64-bit integers is
used.

Non-integer, negative, missing, or non-finite values result in `FALSE`:
there the answer is known, it simply is not "prime".

## Upper limit

Values above `2^53` (`9007199254740992`) return `NA` with a warning,
because for them there is no answer to give. `2^53` is the largest
integer up to which *every* integer is exactly representable; above it
the representable integers thin out, so the value that reaches the test
need not be the value that was entered: R parses `9007199254740997`,
which is prime, as `9007199254740996`. Every representable double above
`2^53` is even, so testing the neighbour would report `FALSE` for
*every* prime beyond the bound – silently, and with no way for the
caller to notice. For larger numbers, use `gmp::isprime()` with a
`gmp::as.bigz()` or character input.

[`factorize`](factorize.md) carries the same bound but rejects the input
with an error instead. The difference is deliberate:
[`factorize()`](factorize.md) answers one number per call element and
can refuse the call, whereas a vectorized predicate should not let a
single unrepresentable element discard the result for all the others.

## See also

Other number.theory: [`GCD-LCM`](GCD-LCM.md),
[`digitSum()`](digitSum.md), [`divisors()`](divisors.md),
[`factorize()`](factorize.md), [`fibonacci()`](fibonacci.md),
[`isOdd()`](isOdd.md), [`primes()`](primes.md)

## Examples

``` r
isPrime(2)
#> [1] TRUE
isPrime(1:10)
#>  [1] FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE
isPrime(c(17, 18, 19))
#> [1]  TRUE FALSE  TRUE
```
