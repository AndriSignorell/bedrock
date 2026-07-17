# Identify Columns Without Missing Values

This function checks each element of a data frame or list-like object
for missing values (`NA`) and identifies those that are completely
observed, i.e., contain no missing entries.

## Usage

``` r
completeColumns(x, output = c("names", "logical"))
```

## Arguments

- x:

  a data.frame or list-like object whose elements are checked for
  missing values.

- output:

  character string specifying the output representation. One of
  `"names"` (return the names of the complete elements, the default) or
  `"logical"` (return a logical vector indicating completeness for each
  element).

## Value

if `output = "names"`, a character vector with the names of all complete
elements.

If `output = "logical"`, a logical vector of length `length(x)`, where
`TRUE` indicates that the corresponding element contains no missing
values.

## Details

An element is considered *complete* if it contains zero missing values.
Internally, the function uses [`anyNA`](https://rdrr.io/r/base/NA.html)
to detect missing values.

## See also

[`anyNA`](https://rdrr.io/r/base/NA.html),
[`is.na`](https://rdrr.io/r/base/NA.html),
[`na.omit`](https://rdrr.io/r/stats/na.fail.html),
[`complete.cases`](https://rdrr.io/r/stats/complete.cases.html)

Other data.missing: [`countCompCases()`](countCompCases.md)

## Examples

``` r
# Names of columns without missing values
completeColumns(airquality)
#> [1] "Wind"  "Temp"  "Month" "Day"  

# Logical vector indicating completeness
completeColumns(airquality, output = "logical")
#>   Ozone Solar.R    Wind    Temp   Month     Day 
#>   FALSE   FALSE    TRUE    TRUE    TRUE    TRUE 
```
