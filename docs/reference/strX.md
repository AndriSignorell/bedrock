# Extended str() with numbered variables

Wrapper around [`utils::str()`](https://rdrr.io/r/utils/str.html) that
optionally numbers variables in lists and data frames. Useful for large
objects where variables should be referenced by position.

## Usage

``` r
strX(object, ..., enumerate = TRUE, recursive = FALSE, strict.width = "cut")
```

## Arguments

- object:

  Any R object.

- ...:

  Additional arguments passed to
  [`utils::str()`](https://rdrr.io/r/utils/str.html).

- enumerate:

  Logical. Should variables/elements be numbered? Default is `TRUE`.

- recursive:

  Logical. Should nested list elements also be numbered? Default is
  `FALSE`.

- strict.width:

  Character string passed to
  [`utils::str()`](https://rdrr.io/r/utils/str.html). Default is
  `"cut"`.

## Value

Invisibly returns the character vector produced by
[`utils::str()`](https://rdrr.io/r/utils/str.html).

## Details

By default, only top-level elements are numbered. Recursive numbering of
nested list elements can be enabled with `recursive = TRUE`.

## Examples

``` r
# Data frame
strX(mtcars)
#> 'data.frame':    32 obs. of  11 variables:
#>   1 $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>   2 $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
#>   3 $ disp: num  160 160 108 258 360 ...
#>   4 $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
#>   5 $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
#>   6 $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
#>   7 $ qsec: num  16.5 17 18.6 19.4 17 ...
#>   8 $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
#>   9 $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
#>  10 $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
#>  11 $ carb: num  4 4 1 1 2 1 4 2 2 4 ...

# Nested list
x <- list(
  a = 1,
  b = list(
    c = 2,
    d = 3
  )
)

strX(x)
#> List of 2
#>  1 $ a: num 1
#>  2 $ b:List of 2
#>   ..$ c: num 2
#>   ..$ d: num 3

# Recursive numbering
strX(x, recursive = TRUE)
#> List of 2
#>  1 $ a: num 1
#>  2 $ b:List of 2
#>   ..$ c: num 2
#>   ..$ d: num 3
```
