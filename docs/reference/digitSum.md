# Digit sum for integer values

Computes the sum of digits for integer inputs. Negative values are
handled by taking the absolute value.

## Usage

``` r
digitSum(x)
```

## Arguments

- x:

  An integer vector.

## Value

An integer vector containing the digit sums.

## Details

The function only accepts integer values. If non-integer numerics are
supplied, an error is thrown. Missing values (`NA`) are propagated.

## See also

Other number.theory: [`factorize()`](factorize.md),
[`fibonacci()`](fibonacci.md), [`isOdd()`](isOdd.md),
[`isPrime()`](isPrime.md),
[`numeric-conversions`](numeric-conversions.md), [`primes()`](primes.md)

## Examples

``` r
digitSum(124L)
#> [1] 7
digitSum(c(10L, 99L, -1234L))
#> [1]  1 18 10
```
