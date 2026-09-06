# Precision, Decimal Places and Fractional Part of a Numeric Value

Four small utilities for the written form of a number, as opposed to its
value.

## Usage

``` r
nDec(x)

maxDec(x)

prec(x)

frac(x)
```

## Arguments

- x:

  a numeric vector, or a character vector of numbers as written.

## Value

- `nDec()`: an integer vector of the same length as `x`; `NA` elements
  yield `NA`.

- `maxDec()`: a single integer value, `0` if `x` has no non-missing
  element with decimals.

- `prec()`: a single numeric value, the finest precision found across
  all (non-missing) elements of `x`. Returns 1 if all values are zero
  and `NA` if no non-missing values are left.

- `frac()`: a numeric vector of the same length as `x`.

## Details

`nDec()` returns the number of decimal places of every element.  
`maxDec()` returns the largest of those numbers.  
`prec()` returns the precision, the smallest positional value of the
last significant digit found in `x` (e.g. 0.001 for 3.142).  
`frac()` returns the fractional part.

`nDec()` and `maxDec()` count what is printed: the input is converted
with [`as.character()`](https://rdrr.io/r/base/character.html), an
exponent is discarded, and the digits behind the last decimal separator
are counted. A number that R chooses to print in scientific notation
therefore has no decimal places, `nDec(1e-300)` is `0`, and trailing
zeros of a numeric are gone before counting, as `1.50` and `1.5` are the
same number. Pass the values as character strings to count them as
written.

Where R switches to scientific notation is R's decision, not this
function's, and it has moved between versions: up to R 4.2
[`as.character()`](https://rdrr.io/r/base/character.html) followed
`options(scipen=)`, since R 4.3 it writes the shortest representation
that reads back as the same number. A value near that switch, such as
`0.00001`, may therefore count five decimals or none, depending on the R
version. Pass it as a character string to fix the count.

Both a period and a comma are accepted as the decimal separator of a
character input, the last one in the string deciding, so that a
thousands separator does not distort the count. Numeric input always
arrives with a period, whatever `getOption("OutDec")` says.

`maxDec(x)` is the maximum of `nDec(x)`, missing values removed, and `0`
when nothing is left to count.

`prec()` works on the value rather than on its written form and reports
the position of the last significant digit across the whole vector, not
one value per element. For input that is exact in decimal it is
`10^-maxDec(x)`.

`frac()` discards the sign, the fractional part of `-1.25` being `0.25`,
as the sign belongs to the integer part of the number. To read the
decimals as an integer, scale and round the result,
`round(1e4 * frac(x))` for the first four of them.

## See also

[`format.info()`](https://rdrr.io/r/base/format.info.html),
[`as.integer()`](https://rdrr.io/r/base/integer.html),
[`trunc()`](https://rdrr.io/r/base/Round.html)

## Examples

``` r

x <- rnorm(5)*100
x
#> [1] -155.70357  192.31637 -185.68296 -210.61184   69.76485
frac(x)
#> [1] 0.7035744 0.3163653 0.6829628 0.6118436 0.7648527

# the first four decimal digits, as an integer
round(1e4 * frac(x))
#> [1] 7036 3164 6830 6118 7649

# the sign belongs to the integer part
frac(c(-1.25, 1.25))
#> [1] 0.25 0.25
## [1] 0.25 0.25

nDec(c(1.25, 1.8, 12.0, 1.00000))
#> [1] 2 1 0 0
## [1] 2 1 0 0

# the same numbers, summarised
maxDec(c(1.25, 1.8, 12.0, 1.00000))
#> Error in maxDec(c(1.25, 1.8, 12, 1)): could not find function "maxDec"
## [1] 2

x <- c("0.0000", "0", "159.283", "1.45e+10", "1.4599E+10" )
nDec(x)
#> [1] 4 0 3 2 4
prec(as.numeric(x))
#> [1] 0.001

# trailing zeros survive in a character input, but not in a numeric one
nDec("1.500")
#> [1] 3
## [1] 3
nDec(1.500)
#> [1] 1
## [1] 1
```
