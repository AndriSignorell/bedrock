# Pairwise Calculations

Implements a logic to run pairwise calculations on the columns of a
data.frame or a matrix.

## Usage

``` r
pairApply(x, FUN = NULL, ..., symmetric = FALSE)
```

## Arguments

- x:

  a list, a data.frame or a matrix with columns to be processed
  pairwise.

- FUN:

  a function (or the name of a function) to be calculated. It is
  assumed, that the first 2 arguments denominate x and y, and that it
  returns a single numeric value.

- ...:

  the dots are passed to FUN

- symmetric:

  logical. Does the function yield the same result for FUN(x, y) and
  FUN(y, x)?  
  If `TRUE` just the lower triangular matrix is calculated and mirrored.
  Default is FALSE.

## Value

a matrix with the results of FUN.

## Details

This code is based on the logic of
[`cor()`](https://rdrr.io/r/stats/cor.html) and extended for asymmetric
functions. Cell `[i, j]` of the result contains
`FUN(x[[i]], x[[j]], ...)`, so the first argument of `FUN` corresponds
to the row variable and the second to the column variable.

## See also

[`base::outer()`](https://rdrr.io/r/base/outer.html),
[stats::pairwise.table](https://rdrr.io/r/stats/pairwise.table.html)

Other combinatorics: [`combN()`](combN.md),
[`combPairs()`](combPairs.md), [`combSet()`](combSet.md),
[`permn()`](permn.md), [`randGroupSplit()`](randGroupSplit.md),
[`sampleX()`](sampleX.md)

## Examples

``` r

# build a dataset
set.seed(1)
d.sub <- transform(
  data.frame(
    X1 = rnorm(n <- 300),
    X3 = rnorm(n)),
  X2 = 0.8*X1 + rnorm(n),
  X4 = 0.5*X3 + rnorm(n)
  )

pairApply(d.sub, FUN = cor, method="spearman")
#>            X1         X3          X2          X4
#> X1 1.00000000 0.03799909  0.55466483  0.01294681
#> X3 0.03799909 1.00000000  0.06833009  0.44778009
#> X2 0.55466483 0.06833009  1.00000000 -0.02556606
#> X4 0.01294681 0.44778009 -0.02556606  1.00000000

# user defined functions are ok as well
pairApply(d.sub,
  FUN = function(x,y)
    wilcox.test(as.numeric(x), as.numeric(y))$p.value, symmetric=TRUE)
#>           X1        X3        X2        X4
#> X1 1.0000000 0.6297484 0.4566135 0.4158320
#> X3 0.6297484 1.0000000 0.7499997 0.7142069
#> X2 0.4566135 0.7499997 1.0000000 0.9359926
#> X4 0.4158320 0.7142069 0.9359926 1.0000000
```
