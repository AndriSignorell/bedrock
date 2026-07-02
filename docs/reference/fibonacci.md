# Fibonacci Numbers

Generate Fibonacci numbers. The Fibonacci numbers can also be calculated
using the golden ratio `phi`, as demonstrated in the examples.

## Usage

``` r
fibonacci(n)
```

## Arguments

- n:

  nonnegative integer or vector of nonnegative integers.

## Value

A single integer, or a vector of integers.

## Details

Generates the `n`-th Fibonacci number, whereas `Fibonacci(0) = 0`.  
The golden ratio is defined as `phi = 0.5*(1+sqrt(5))`.

## References

<https://en.wikipedia.org/wiki/Fibonacci_number>  
<https://mathworld.wolfram.com/GoldenRatio.html>

## See also

Other number.theory: [`digitSum()`](digitSum.md),
[`divisors()`](divisors.md), [`factorize()`](factorize.md),
[`gcd_lcm`](gcd_lcm.md), [`isOdd()`](isOdd.md),
[`isPrime()`](isPrime.md),
[`numeric-conversions`](numeric-conversions.md), [`primes()`](primes.md)

## Examples

``` r

fibonacci(0)                            # 1
#> [1] 0
fibonacci(2)                            # 2
#> [1] 1
fibonacci(0:3)                          # 0 1 1 2
#> [1] 0 1 1 2
fibonacci(0:25)                         # ... 75025 121393
#>  [1]     0     1     1     2     3     5     8    13    21    34    55    89
#> [13]   144   233   377   610   987  1597  2584  4181  6765 10946 17711 28657
#> [25] 46368 75025

# Golden ratio = Fib(25)/ Fib(24)
f25 <- quot(fibonacci(24:25))           # 1.618033989
phi <- (sqrt(5) + 1)/2
abs(f25 - phi)                          # 7.945178e-11
#> [1] 2.080072e-10

# Fibonacci numbers without iteration
fibo <- function(n) {
  phi <- (sqrt(5) + 1)/2
  fib <- (phi^(n+1) - (1-phi)^(n+1)) / (2*phi - 1)
  round(fib)
}
  
fibo(30:33)                             # 1346269 2178309 3524578 5702887
#> [1] 1346269 2178309 3524578 5702887
```
