# Set the Names in an Object

This is a convenience function that sets the names of an object and
returns it including the new names. It is most useful at the end of a
function definition where one is creating the object to be returned and
would prefer not to store it under a name just that the names can be
assigned. In addition to the function
[`setNames`](https://rdrr.io/r/stats/setNames.html) in base R the user
can decide, whether rownames, colnames or simply the names are to be
set. Names are recyled.

## Usage

``` r
setNamesX(x, ...)
```

## Arguments

- x:

  an object for which a names attribute will be meaningful

- ...:

  the names to be assigned to the object. This should be a character
  vector of names named `dimnames`, `rownames`, `colnames` or `names`.
  Setting `rownames=NULL` would remove existing rownames. All kind of
  names can be changed at the same time. Default would be `names`.
  Abbreviations are supported.

## Value

An object of the same sort as object with the new names assigned.

## See also

[`setNames`](https://rdrr.io/r/stats/setNames.html)

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md),
[`columnWrap()`](columnWrap.md), [`combLevels()`](combLevels.md),
[`nf()`](nf.md), [`parseSASDatalines()`](parseSASDatalines.md),
[`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`revCode()`](revCode.md), [`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`sortX()`](sortX.md), [`splitX()`](splitX.md),
[`stringsAsFactors()`](stringsAsFactors.md), [`toBaseR()`](toBaseR.md),
[`untable()`](untable.md)

## Examples

``` r

setNamesX(1:5, names=letters[1:5])
#> a b c d e 
#> 1 2 3 4 5 

# the default, if no argument names are provided, is "names"
setNamesX(1:5, letters[1:5])
#> a b c d e 
#> 1 2 3 4 5 

# rownames and columnnames can be set at the same time
setNamesX(matrix(c(1:12), nrow=4), 
         rownames=LETTERS[11:14], colnames=c("perc", "lci", "uci"))
#>   perc lci uci
#> K    1   5   9
#> L    2   6  10
#> M    3   7  11
#> N    4   8  12
         
# can also be used to set the names to an empty string
setNamesX(diag(6), rownames="", colnames="")
#>             
#>  1 0 0 0 0 0
#>  0 1 0 0 0 0
#>  0 0 1 0 0 0
#>  0 0 0 1 0 0
#>  0 0 0 0 1 0
#>  0 0 0 0 0 1

# setting dimnames works as well
tab <- setNamesX(
  as.table(rbind(c(84,43), c(10,92))), 
    dimnames= list(
       dipstick=c("positive","negative"),
       culture=c("positive","negative")))

```
