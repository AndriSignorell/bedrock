# Prime Factorization of Integers

Compute the prime factorization(s) of integer(s) `n`, using Pollard's
rho algorithm with deterministic Miller-Rabin primality testing (64-bit,
implemented in C++).

## Usage

``` r
factorize(n)
```

## Arguments

- n:

  vector of positive whole numbers to factorize, not exceeding `2^53`.

## Value

a named [`list`](https://rdrr.io/r/base/list.html) of the same length as
`n`, each element a 2-column matrix with column `"p"` the prime factors
and column `"m"` their respective exponents (or multiplicities), i.e.,
for a prime number `n`, the resulting matrix is `cbind(p = n, m = 1)`.

## Details

`n` must not exceed `2^53` (`.Machine$integer.max` squared, roughly
`9.007e15`). Above this bound, R's double representation can no longer
store integers exactly, so a factorized result could silently correspond
to a different number than the one entered – for such inputs, use the
gmp package's `gmp::factorize()`, which represents arbitrarily large
integers exactly (e.g. via `gmp::as.bigz()` or a string).

## See also

Other number.theory: [`GCD-LCM`](GCD-LCM.md),
[`digitSum()`](digitSum.md), [`divisors()`](divisors.md),
[`fibonacci()`](fibonacci.md), [`isOdd()`](isOdd.md),
[`isPrime()`](isPrime.md), [`primes()`](primes.md)

## Examples

``` r

factorize(47)
#> $`47`
#>       p m
#> [1,] 47 1
#> 
factorize(seq(101, 120, by=2))
#> $`101`
#>        p m
#> [1,] 101 1
#> 
#> $`103`
#>        p m
#> [1,] 103 1
#> 
#> $`105`
#>      p m
#> [1,] 2 2
#> [2,] 5 2
#> 
#> $`107`
#>        p m
#> [1,] 107 1
#> 
#> $`109`
#>        p m
#> [1,] 109 1
#> 
#> $`111`
#>       p m
#> [1,]  3 1
#> [2,] 37 1
#> 
#> $`113`
#>        p m
#> [1,] 113 1
#> 
#> $`115`
#>       p m
#> [1,]  5 1
#> [2,] 23 1
#> 
#> $`117`
#>       p m
#> [1,]  3 2
#> [2,] 13 1
#> 
#> $`119`
#>       p m
#> [1,]  7 1
#> [2,] 17 1
#> 
```
