# Prime Factorization of Integers

Compute the prime factorization(s) of integer(s) `n`. Prime
factorization of integer(s) works via [`primes`](primes.md), currently
in a cheap way, sub-optimal for large composite `n`.

## Usage

``` r
factorize(n)
```

## Arguments

- n:

  vector of integers to factorize.

## Value

A named [`list`](https://rdrr.io/r/base/list.html) of the same length as
`n`, each element a 2-column matrix with column `"p"` the prime factors
and column~`"m"` their respective exponents (or multiplities), i.e., for
a prime number `n`, the resulting matrix is `cbind(p = n, m = 1)`.

For factorization of moderately or really large numbers, see the gmp
package, and its
[`factorize()`](https://rdrr.io/pkg/gmp/man/factor.html) (which is ~20x
faster!).

## See also

Other number.theory: [`digitSum()`](digitSum.md),
[`divisors()`](divisors.md), [`fibonacci()`](fibonacci.md),
[`gcd_lcm`](gcd_lcm.md), [`isOdd()`](isOdd.md),
[`isPrime()`](isPrime.md),
[`numeric-conversions`](numeric-conversions.md), [`primes()`](primes.md)

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
