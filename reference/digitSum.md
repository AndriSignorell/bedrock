# Digit Sum for Integer Values

Computes the sum of digits for whole-numbered inputs. Negative values
are handled by taking the absolute value.

## Usage

``` r
digitSum(x)
```

## Arguments

- x:

  an integer vector, or a numeric vector of whole numbers.

## Value

an integer vector containing the digit sums.

## Details

The function accepts integer vectors as well as doubles holding whole
numbers (e.g. `124` and `124L` are both valid). Fractional values raise
an error. Missing values (`NA`) are propagated.

## See also

Other number.theory:
[`GCD-LCM`](https://andrisignorell.github.io/bedrock/reference/GCD-LCM.md),
[`divisors()`](https://andrisignorell.github.io/bedrock/reference/divisors.md),
[`factorize()`](https://andrisignorell.github.io/bedrock/reference/factorize.md),
[`fibonacci()`](https://andrisignorell.github.io/bedrock/reference/fibonacci.md),
[`isOdd()`](https://andrisignorell.github.io/bedrock/reference/isOdd.md),
[`isPrime()`](https://andrisignorell.github.io/bedrock/reference/isPrime.md),
[`primes()`](https://andrisignorell.github.io/bedrock/reference/primes.md)

## Examples

``` r
digitSum(124)
#> [1] 7
digitSum(c(10L, 99L, -1234L))
#> [1]  1 18 10
```
