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

It also holds when one of the values is zero, and that is the shortest
way to see why `LCM(0, 6)` has to be 0 rather than 6.

## Zero

Zero behaves differently in the two functions, which is why they do not
treat it the same way. For the greatest common divisor it is *neutral* -
every number divides 0, so `GCD(0, a)` is `abs(a)` and zeros can simply
be dropped. For the least common multiple it is *absorbing* - 0 is a
multiple of every number and the smallest non-negative one, so
`LCM(0, a)` is 0. `GCD(0, 0)` and `LCM(0, 0)` are both 0.

## See also

Other number.theory:
[`digitSum()`](https://andrisignorell.github.io/bedrock/reference/digitSum.md),
[`divisors()`](https://andrisignorell.github.io/bedrock/reference/divisors.md),
[`factorize()`](https://andrisignorell.github.io/bedrock/reference/factorize.md),
[`fibonacci()`](https://andrisignorell.github.io/bedrock/reference/fibonacci.md),
[`isOdd()`](https://andrisignorell.github.io/bedrock/reference/isOdd.md),
[`isPrime()`](https://andrisignorell.github.io/bedrock/reference/isPrime.md),
[`primes()`](https://andrisignorell.github.io/bedrock/reference/primes.md)

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

# zero is neutral for the GCD and absorbing for the LCM
GCD(0, 6)
#> [1] 6
LCM(0, 6)
#> [1] 0

# n * m == GCD(n, m) * LCM(n, m), zero included
GCD(0, 6) * LCM(0, 6)
#> [1] 0
```
