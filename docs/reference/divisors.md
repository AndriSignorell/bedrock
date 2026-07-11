# Calculate Divisors

Calculate the proper divisors of positive natural numbers.

## Usage

``` r
divisors(x)
```

## Arguments

- x:

  vector of positive whole numbers for which the divisors are to be
  returned

## Value

an integer vector containing the proper divisors if `x` is a single
number, otherwise a named list of such vectors

## Details

Divisibility is a mathematical relationship between two integers. An
integer is divisible by another integer if there is no remainder in the
division. This function returns the *proper* divisors of `x`, i.e. all
positive divisors excluding `x` itself. The number 11 is prime and has
only the proper divisor 1, whereas the number 12 has the proper divisors
1, 2, 3, 4 and 6. In elementary number theory, the concept of
divisibility is limited to natural numbers. The number of proper
divisors can be determined with the function
[`length()`](https://rdrr.io/r/base/length.html).

## See also

Other number.theory: [`GCD-LCM`](GCD-LCM.md),
[`digitSum()`](digitSum.md), [`factorize()`](factorize.md),
[`fibonacci()`](fibonacci.md), [`isOdd()`](isOdd.md),
[`isPrime()`](isPrime.md), [`primes()`](primes.md)

## Examples

``` r

divisors(786)
#> [1]   1   2   3   6 131 262 393

divisors(c(145, 786))
#> $`145`
#> [1]  1  5 29
#> 
#> $`786`
#> [1]   1   2   3   6 131 262 393
#> 
```
