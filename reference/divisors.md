# Calculate Divisors

Calculate the proper divisors of positive natural numbers.

## Usage

``` r
divisors(x)
```

## Arguments

- x:

  vector of positive whole numbers for which the divisors are to be
  returned.

## Value

an integer vector containing the proper divisors in ascending order if
`x` is a single number, otherwise a named list of such vectors. A prime
number yields `1`, and 1 itself yields `integer(0)` - its only divisor
is 1, which is `x` itself and therefore not a proper one.

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

Other number.theory:
[`GCD-LCM`](https://andrisignorell.github.io/bedrock/reference/GCD-LCM.md),
[`digitSum()`](https://andrisignorell.github.io/bedrock/reference/digitSum.md),
[`factorize()`](https://andrisignorell.github.io/bedrock/reference/factorize.md),
[`fibonacci()`](https://andrisignorell.github.io/bedrock/reference/fibonacci.md),
[`isOdd()`](https://andrisignorell.github.io/bedrock/reference/isOdd.md),
[`isPrime()`](https://andrisignorell.github.io/bedrock/reference/isPrime.md),
[`primes()`](https://andrisignorell.github.io/bedrock/reference/primes.md)

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

# the number of proper divisors
length(divisors(786))
#> [1] 7

# a prime has only one, and this one is at the integer limit
divisors(.Machine$integer.max)
#> [1] 1
```
