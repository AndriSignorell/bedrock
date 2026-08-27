# Convert Character Columns to Factors

Strings as factors have recently been downgraded in base R's
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) function.
However, it is still usually a good idea to encode string variables as
factors. This function helps to convert some or all columns of a
data.frame to factors.

## Usage

``` r
stringsAsFactors(x, columns = NULL)
```

## Arguments

- x:

  the data.frame.

- columns:

  names or indexes of the columns to be converted; negative values can
  be used to omit columns.

## Value

the given data.frame including the converted factors.

## See also

Other data.recode: [`asBinary()`](asBinary.md),
[`combLevels()`](combLevels.md), [`dummy()`](dummy.md),
[`mReplace()`](mReplace.md), [`nf()`](nf.md), [`recodeX()`](recodeX.md),
[`revCode()`](revCode.md)

## Examples

``` r
d.dat <- data.frame(char_x = LETTERS[1:5],
                    char_y = LETTERS[6:10],
                    n = 1:5)

# all character columns
str(stringsAsFactors(d.dat))
#> 'data.frame':    5 obs. of  3 variables:
#>  $ char_x: Factor w/ 5 levels "A","B","C","D",..: 1 2 3 4 5
#>  $ char_y: Factor w/ 5 levels "F","G","H","I",..: 1 2 3 4 5
#>  $ n     : int  1 2 3 4 5
# only char_y
str(stringsAsFactors(d.dat, columns = "char_y"))
#> 'data.frame':    5 obs. of  3 variables:
#>  $ char_x: chr  "A" "B" "C" "D" ...
#>  $ char_y: Factor w/ 5 levels "F","G","H","I",..: 1 2 3 4 5
#>  $ n     : int  1 2 3 4 5
# only char_x
str(stringsAsFactors(d.dat, columns = "char_x"))
#> 'data.frame':    5 obs. of  3 variables:
#>  $ char_x: Factor w/ 5 levels "A","B","C","D",..: 1 2 3 4 5
#>  $ char_y: chr  "F" "G" "H" "I" ...
#>  $ n     : int  1 2 3 4 5
# all character columns except the second one ("char_y")
str(stringsAsFactors(d.dat, columns = -2))
#> 'data.frame':    5 obs. of  3 variables:
#>  $ char_x: Factor w/ 5 levels "A","B","C","D",..: 1 2 3 4 5
#>  $ char_y: chr  "F" "G" "H" "I" ...
#>  $ n     : int  1 2 3 4 5
```
