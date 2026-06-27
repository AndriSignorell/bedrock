# Converts Numbers Between Bases

These functions convert numbers from one base to another. There are
several solutions for this problem out there, but the naming is quite
heterogeneous and so consistent function names might be helpful.

## Usage

``` r
hexToDec(x)

decToHex(x)

octToDec(x)

decToOct(x)

binToDec(x)

decToBin(x)

romanToInt(x)

baseToBase(x, from, to, width = NULL)
```

## Arguments

- x:

  A vector of numbers or character representations to be converted.

- from:

  A single integer in \\\[2, 36\]\\ giving the input base (`baseToBase`
  only).

- to:

  A single integer in \\\[2, 36\]\\ giving the output base (`baseToBase`
  only).

- width:

  A single non-negative integer or `NULL` (default). If supplied, the
  output is left-padded with zeros to at least `width` characters
  (`baseToBase` only).

## Value

A numeric or character vector of the same length as `x` containing the
converted values. Binary, octal and decimal values are numeric; hex
values are returned as class `hexmode`. `baseToBase()` always returns a
character vector (uppercase). `NA` input produces `NA` output.

## Details

`binToDec()` converts numbers from binary mode into decimal values;
`decToBin()` does the reverse. Oct means octal system and hex
hexadecimal. `baseToBase()` is the general case, supporting any base
from 2 to 36.

All specialist functions are special cases of `baseToBase()`:

|               |                         |
|---------------|-------------------------|
| `hexToDec(x)` | `baseToBase(x, 16, 10)` |
| `decToHex(x)` | `baseToBase(x, 10, 16)` |
| `octToDec(x)` | `baseToBase(x, 8, 10)`  |
| `decToOct(x)` | `baseToBase(x, 10, 8)`  |
| `binToDec(x)` | `baseToBase(x, 2, 10)`  |
| `decToBin(x)` | `baseToBase(x, 10, 2)`  |

`baseToBase()` uses [`strtoi()`](https://rdrr.io/r/base/strtoi.html) for
parsing, which operates on `long int` internally. Values above
approximately \\2^{31} - 1\\ may return `NA` on 32-bit platforms.

## See also

[`strtoi`](https://rdrr.io/r/base/strtoi.html),
[`as.hexmode`](https://rdrr.io/r/base/hexmode.html),
[`as.octmode`](https://rdrr.io/r/base/octmode.html),
[`as.roman`](https://rdrr.io/r/utils/roman.html)

Other number.theory: [`digitSum()`](digitSum.md),
[`factorize()`](factorize.md), [`fibonacci()`](fibonacci.md),
[`isOdd()`](isOdd.md), [`isPrime()`](isPrime.md),
[`primes()`](primes.md)

## Examples

``` r
decToBin(c(17, 25))
#> [1] "10001" "11001"
binToDec(c(101, 11101))
#> [1]  5 29

decToOct(c(17, 25))
#> [1] 21 31
octToDec(c(11, 77))
#> [1]  9 63

decToHex(c(17, 25))
#> [1] "11" "19"
hexToDec(c("FF", "AA", "ABC"))
#> [1]  255  170 2748

# general base conversion
baseToBase("FF",       from = 16, to = 10)   # hex -> dec
#> [1] "255"
baseToBase("255",      from = 10, to = 16)   # dec -> hex
#> [1] "FF"
baseToBase("11111111", from = 2,  to = 10)   # bin -> dec
#> [1] "255"
baseToBase("1A3F",     from = 16, to = 2)    # hex -> bin
#> [1] "1101000111111"
baseToBase("Z9",       from = 36, to = 10)   # base-36 -> dec
#> [1] "1269"

# fixed-width output
baseToBase(c("0", "7", "255"), from = 10, to = 2, width = 8)
#> [1] "00000000" "00000111" "11111111"

# vectorized
baseToBase(c("A", "B", "FF"), from = 16, to = 10)
#> [1] "10"  "11"  "255"
```
