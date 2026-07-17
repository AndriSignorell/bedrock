# Recycle a List of Elements

This function recycles all supplied elements to the maximal dimension.

## Usage

``` r
recycle(..., maxdim = NULL, strict = FALSE)
```

## Arguments

- ...:

  a number of vectors of elements.

- maxdim:

  defines the maximal dimension, if set to `NULL` (default) the maximal
  dimension of the list.

- strict:

  logical, if `TRUE` each element must have length 1 or `maxdim`, so
  that no partial recycling (or truncation) can occur. Default is
  `FALSE`.

## Value

a list of the supplied elements  
`attr(,"maxdim")` contains the maximal dimension of the recycled list.

## Details

If `maxdim` is smaller than the length of an element, that element is
truncated to the first `maxdim` values. Zero-length elements are
recycled to `NA` vectors of length `maxdim`. Both situations are
rejected when `strict = TRUE`.

## See also

[`rep`](https://rdrr.io/r/base/rep.html),
[`replicate`](https://rdrr.io/r/base/lapply.html)

Other pkg.args: [`callIf()`](callIf.md),
[`extractArgs()`](extractArgs.md), [`getDotsArg()`](getDotsArg.md),
[`mergeArgs()`](mergeArgs.md)

## Examples

``` r

recycle(x=1:5, y=1, s=letters[1:2])
#> $x
#> [1] 1 2 3 4 5
#> 
#> $y
#> [1] 1 1 1 1 1
#> 
#> $s
#> [1] "a" "b" "a" "b" "a"
#> 
#> attr(,"maxdim")
#> [1] 5

z <- recycle(x=letters[1:5], n=2:3, sep=c("-"," "))
sapply(1:attr(z, "maxdim"), function(i) paste(rep(z$x[i], times=z$n[i]),
                                        collapse=z$sep[i]))
#> [1] "a-a"   "b b b" "c-c"   "d d d" "e-e"  
```
