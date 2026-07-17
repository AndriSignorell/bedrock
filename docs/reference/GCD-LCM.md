# Greatest Common Divisor and Least Common Multiple

Calculates the greatest common divisor (GCD) and least common multiple
(LCM) of all the values present in its arguments.

## Usage

``` r
GCD(..., na.rm = FALSE)

LCM(..., na.rm = FALSE)
```

## Arguments

- ...:

  integer or logical vectors.

- na.rm:

  logical; whether missing values (including NaN) are removed.

## Value

a numeric (integer) value.

## Details

The computation is based on the Euclidean algorithm without using the
extended version. The greatest common divisor for all numbers in the
integer vector `x` will be computed (the multiple GCD). Negative values
are allowed and enter via their absolute value; logical vectors are
coerced to integer.

## Note

The following relation is always true:

`n * m = GCD(n, m) * LCM(n, m)`

## See also

Other number.theory: [`digitSum()`](digitSum.md),
[`divisors()`](divisors.md), [`factorize()`](factorize.md),
[`fibonacci()`](fibonacci.md), [`isOdd()`](isOdd.md),
[`isPrime()`](isPrime.md), [`primes()`](primes.md)

## Examples

``` r
GCD(12, 10)
#> [1] 2
GCD(144, 233)    # Fibonacci numbers are relatively prime to each other
#> [1] 1

LCM(12, 10)
#> [1] 60
LCM(144, 233)    # = 144 * 233
#> [1] 33552

# all elements will be flattened by unlist
GCD(2, 3, c(5, 7) * 11)
#> [1] 1
GCD(c(2*3, 3*5, 5*7))
#> [1] 1
LCM(c(2, 3, 5, 7) * 11)
#> [1] 2310
LCM(2*3, 3*5, 5*7)
#> [1] 210
```
