# Calculate Divisors

Calculate divisors of positive natural numbers.

## Usage

``` r
divisors(x)
```

## Arguments

- x:

  integer number for which the divisors are to be returned

## Value

an integer vector containg the divisors

## Details

Divisibility is a mathematical relationship between two integers. An
integer is divisible by another integer if there is no remainder in the
division. The number 11 has only two divisors: 1 and the number 11
itself, whereas the number 12 has many divisors: 1, 2, 3, 4, 6 and 12.
In elementary number theory, the concept of divisibility is limited to
natural numbers. The number of its divisors can be determined with the
function [`length()`](https://rdrr.io/r/base/length.html).

## See also

Other math.utils: [`crossProd()`](crossProd.md),
[`crossProdN()`](crossProdN.md), [`dotProd()`](dotProd.md),
[`gcd_lcm`](gcd_lcm.md), [`percentRank()`](percentRank.md),
[`precision`](precision.md), [`ptInPoly()`](ptInPoly.md),
[`roundTo()`](roundTo.md), [`unirootAll()`](unirootAll.md)

## Author

Andri Signorell <andri@signorell.net>

## Examples

``` r

divisors(c(145, 786))
#> [[1]]
#> [1]  1  5 29
#> 
#> [[2]]
#> [1]   1   2   3   6 131 262 393
#> 
```
