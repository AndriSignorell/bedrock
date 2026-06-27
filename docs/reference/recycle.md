# Recyle a List of Elements

This function recycles all supplied elments to the maximal dimension.

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

  defines if number of arguments must be 1 or maxdim.

## Value

a list of the supplied elements  
`attr(,"maxdim")` contains the maximal dimension of the recyled list

## See also

[`rep`](https://rdrr.io/r/base/rep.html),
[`replicate`](https://rdrr.io/r/base/lapply.html)

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md),
[`columnWrap()`](columnWrap.md), [`combLevels()`](combLevels.md),
[`nf()`](nf.md), [`parseSASDatalines()`](parseSASDatalines.md),
[`recodeX()`](recodeX.md), [`revCode()`](revCode.md),
[`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`splitX()`](splitX.md), [`stringsAsFactors()`](stringsAsFactors.md),
[`toBaseR()`](toBaseR.md), [`untable()`](untable.md)

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
