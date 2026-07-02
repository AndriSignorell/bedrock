# Character \<-\> ASCII Conversion

Convert characters to ASCII codes and vice versa.

## Usage

``` r
charToAscii(x, output = c("vector", "list"))

asciiToChar(i)
```

## Arguments

- x:

  Character vector.

- output:

  Character string specifying the output representation. One of:

  `\"vector\"`

  :   Return a simplified integer vector when possible.

  `\"list\"`

  :   Always return a list of integer vectors.

  Default is `\"vector\"`.

- i:

  Integer vector of ASCII codes (1–255).

## Value

- `charToAscii()` returns either an integer vector or a list of integer
  vectors depending on `output`.

- `asciiToChar()` returns a character vector.

## Details

`charToAscii()` converts each character in a string to its ASCII code.

`asciiToChar()` converts ASCII codes back to characters.

Only codes in `1:127` are standard ASCII and consistent across systems.

Codes above 127 depend on the current locale and encoding (e.g.
ISO-8859-1).

Note that `0` (NUL) is not supported in R character strings.

The `output` argument controls the representation returned by
`charToAscii()`:

- `\"vector\"`:

  Simplify the result when possible:

      \itemize{
        \item single string -> integer vector
        \item all strings length 1 -> integer vector
        \item otherwise -> list
      }

- `\"list\"`:

  Always return a list of integer vectors.

## See also

[`charToRaw`](https://rdrr.io/r/base/rawConversion.html),
[`rawToChar`](https://rdrr.io/r/base/rawConversion.html)

Other string.utilities: [`mGsub()`](mGsub.md),
[`mReplace()`](mReplace.md), [`strSplitToCol()`](strSplitToCol.md),
[`strSplitToDummy()`](strSplitToDummy.md)

## Examples

``` r
# basic usage
x <- charToAscii("Silvia")
x
#> [1]  83 105 108 118 105  97

asciiToChar(x)
#> [1] "S" "i" "l" "v" "i" "a"

# multiple strings
charToAscii(c("A", "BC"), output = "list")
#> [[1]]
#> [1] 65
#> 
#> [[2]]
#> [1] 66 67
#> 

# split into individual characters
strsplit(asciiToChar(x), split = NULL)
#> [[1]]
#> [1] "S"
#> 
#> [[2]]
#> [1] "i"
#> 
#> [[3]]
#> [1] "l"
#> 
#> [[4]]
#> [1] "v"
#> 
#> [[5]]
#> [1] "i"
#> 
#> [[6]]
#> [1] "a"
#> 

# comparison with raw representation
charToRaw("Silvia")
#> [1] 53 69 6c 76 69 61
```
