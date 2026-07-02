# Fractional Part and Maximal Digits of a Numeric Value

`frac()` returns the fractional part of a numeric value. `maxDigits()`
return the number of digits in `x`.  
`nDec()` returns the number of decimals.  
`prec()` returns the precision of a number `x`.

## Usage

``` r
nDec(x)

prec(x)

frac(x, dpwr = NA)

maxDigits(x)
```

## Arguments

- x:

  the numeric value (or a vector of numerics), whose fractional part is
  to be calculated.

- dpwr:

  power of 10 for a factor z, the fractional part will be multiplied
  with. The result will be returned rounded to integer. Defaults to `NA`
  and will then be ignored.

## See also

[`format.info`](https://rdrr.io/r/base/format.info.html),
[`as.integer`](https://rdrr.io/r/base/integer.html),
[`trunc`](https://rdrr.io/r/base/Round.html)

Other math.utils: [`crossProd()`](crossProd.md),
[`crossProdN()`](crossProdN.md), [`dotProd()`](dotProd.md),
[`linScale()`](linScale.md), [`logit()`](logit.md),
[`nUnique()`](nUnique.md), [`percentRank()`](percentRank.md),
[`rankX()`](rankX.md), [`roundTo()`](roundTo.md),
[`unirootAll()`](unirootAll.md), [`untable()`](untable.md)

## Examples

``` r

x <- rnorm(5)*100
x
#> [1] -155.70357  192.31637 -185.68296 -210.61184   69.76485
frac(x)
#> [1] 0.7035744 0.3163653 0.6829628 0.6118436 0.7648527

# multiply by 10^4
frac(x, dpwr=4)
#> [1] 7036 3164 6830 6118 7649

maxDigits(c(1.25, 1.8, 12.0, 1.00000))
#> [1] 2

x <- c("0.0000", "0", "159.283", "1.45e+10", "1.4599E+10" )
nDec(x)
#> [1] 4 0 3 2 4
prec(as.numeric(x))
#> [1] 0.001
```
