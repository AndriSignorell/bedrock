# Convert Numbers Between Bases

Vectorized conversion between positional numeral systems (bases 2-36),
plus Roman-numeral parsing. The convenience wrappers cover the most
common cases; `baseToBase()` handles any combination of bases.

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

  a vector of numbers or character strings representing values in the
  input base. For `baseToBase()` a numeric `x` is accepted only when
  `from = 10`. `NA` propagates to the output.

- from:

  a single integer in \\\[2, 36\]\\ specifying the input base
  (`baseToBase()` only)

- to:

  a single integer in \\\[2, 36\]\\ specifying the output base
  (`baseToBase()` only)

- width:

  a single non-negative integer or `NULL` (default). When given, output
  strings are left-padded with zeros to at least `width` characters
  (`baseToBase()` only).

## Value

A vector of the same length as `x`:

- `binToDec()`, `octToDec()`, `hexToDec()`, `romanToInt()` - integer or
  numeric vector.

- `decToHex()` - object of class
  [`hexmode`](https://rdrr.io/r/base/hexmode.html).

- `decToOct()` - numeric vector (octal digit string coerced to numeric).

- `decToBin()`, `baseToBase()` - character vector (uppercase digits).

`NA` input always produces `NA` output.

## Convenience wrappers

All specialist functions are thin wrappers around `baseToBase()`:

|               |                         |                        |
|---------------|-------------------------|------------------------|
| **Function**  | **Equivalent call**     | **Returns**            |
| `binToDec(x)` | `baseToBase(x, 2, 10)`  | integer                |
| `decToBin(x)` | `baseToBase(x, 10, 2)`  | character              |
| `octToDec(x)` | `baseToBase(x, 8, 10)`  | integer                |
| `decToOct(x)` | `baseToBase(x, 10, 8)`  | numeric (octal digits) |
| `hexToDec(x)` | `baseToBase(x, 16, 10)` | integer                |
| `decToHex(x)` | `baseToBase(x, 10, 16)` | `hexmode`              |

`hexToDec()` additionally strips a leading `#` from CSS-style colour
strings.

## Roman numerals

`romanToInt()` converts Roman numeral strings (e.g. `"XIV"`) to
integers. Input is trimmed and upper-cased before parsing; invalid
strings return `NA`. See also base R's
[`as.roman()`](https://rdrr.io/r/utils/roman.html) for the reverse
direction.

## Platform limits

`baseToBase()` uses [`strtoi()`](https://rdrr.io/r/base/strtoi.html)
internally, which operates on `long int`. On 32-bit platforms values
above \\2^{31} - 1\\ may silently return `NA`. `decToBin()` applies the
same cap explicitly (values \\\> 536\\870\\911\\ become `NA`).

## See also

[`strtoi`](https://rdrr.io/r/base/strtoi.html),
[`as.hexmode`](https://rdrr.io/r/base/hexmode.html),
[`as.octmode`](https://rdrr.io/r/base/octmode.html),
[`as.roman`](https://rdrr.io/r/utils/roman.html)

## Examples

``` r
# binary
decToBin(c(0, 1, 17, 255))
#> [1] "0"        "1"        "10001"    "11111111"
binToDec(c("0", "1", "10001", "11111111"))
#> [1]   0   1  17 255

# octal
decToOct(c(8, 64, 255))
#> [1]  10 100 377
octToDec(c(10, 100, 377))
#> [1]   8  64 255

# hexadecimal  (CSS colour strings are also accepted by hexToDec)
decToHex(c(0, 255, 65535))
#> [1] "0000" "00ff" "ffff"
hexToDec(c("FF", "ff", "#1A2B3C"))
#> [1]     255     255 1715004

# Roman numerals
romanToInt(c("I", "IV", "XIV", "MCMXCIX"))   # 1, 4, 14, 1999
#> [1]    1    4   14 1999
romanToInt("invalid")                          # NA
#> [1] NA

# baseToBase: general case
baseToBase("FF",       from = 16, to = 10)   # 255
#> [1] "255"
baseToBase("255",      from = 10, to = 16)   # "FF"
#> [1] "FF"
baseToBase("11111111", from =  2, to = 10)   # 255
#> [1] "255"
baseToBase("1A3F",     from = 16, to =  2)   # binary expansion
#> [1] "1101000111111"
baseToBase("Z9",       from = 36, to = 10)   # base-36 -> decimal
#> [1] "1269"

# fixed-width padding (useful for bit-pattern alignment)
baseToBase(c(0, 7, 255), from = 10, to = 2, width = 8)
#> [1] "00000000" "00000111" "11111111"

# vectorized over x
baseToBase(c("A", "B", "FF"), from = 16, to = 10)
#> [1] "10"  "11"  "255"
```
