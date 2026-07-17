# Character \<-\> ASCII Conversion

Convert characters to their numeric character codes and vice versa.

## Usage

``` r
charToAscii(x, output = c("vector", "list"))

asciiToChar(i)
```

## Arguments

- x:

  a character vector.

- output:

  character string specifying the output representation. One of
  `"vector"` (simplify the result whenever possible, the default) or
  `"list"` (always return a list). See Details.

- i:

  an integer vector of character codes (1–255).

## Value

- `charToAscii()` returns either an integer vector or a list of integer
  vectors, depending on `output`.

- `asciiToChar()` returns a character vector.

## Details

`charToAscii()` converts each character in a string to its corresponding
numeric code.

`asciiToChar()` converts numeric codes back to characters.

Only values in the range `1:127` belong to the ASCII standard and
therefore have the same meaning across all systems. Values `128:255`
depend on the current character encoding (for example ISO-8859-1 or
Windows-1252) and may produce different characters on different
platforms.

Note that `0` (NUL) cannot be represented in R character strings and is
therefore not supported.

The `output` argument controls the representation returned by
`charToAscii()`:

- `"vector"`:

  Simplifies the result whenever possible.

  Returns an integer vector if:

  - the input consists of a single string, or

  - all input strings have length one.

  Otherwise, a list of integer vectors is returned.

- `"list"`:

  Always returns a list of integer vectors.

## See also

[`charToRaw`](https://rdrr.io/r/base/rawConversion.html),
[`rawToChar`](https://rdrr.io/r/base/rawConversion.html)

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
