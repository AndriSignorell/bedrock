# Return the First Element Not Being NA

If several vectors are supplied, the evaluation will be elementwise,
resp. rowwise if x is a data.frame or a matrix. The first element of the
result is the first non `NA` element of the first elements of all the
arguments, the second element of the result is the one of the second
elements of all the arguments and so on.  
Shorter inputs (of non-zero length) are NOT recycled: if all inputs have
length greater than 1, they must have the same length (the function will
bark otherwise). If any input has length 1 or 0, all inputs are
flattened into a single vector (dropping `NULL`s) and the first valid
element is returned, in the manner of a scalar SQL `COALESCE`.  
The idea is borrowed from SQL. Might sometimes be useful when preparing
data in R instead of in SQL.

## Usage

``` r
coalesceX(..., method = c("is.na", "is.null", "is.finite"), flatten = TRUE)
```

## Arguments

- ...:

  the elements to be evaluated. This can either be a single vector,
  several vectors of same length, a matrix, a data.frame or a list of
  vectors (of same length). See examples.

- method:

  one out of `"is.na"` (default), `"is.null"` or `"is.finite"`. With
  `"is.na"`, `Inf` values are treated as valid; with `"is.finite"` they
  are skipped. `"is.null"` operates on the arguments themselves and
  returns the first one that is not `NULL`.

- flatten:

  logical, defines whether lists are going to be flattened (default
  `TRUE`).

## Value

return a single vector of the first non `NA` element(s) of the given
data structure.

## See also

[`is.na`](https://rdrr.io/r/base/NA.html),
[`is.finite`](https://rdrr.io/r/base/is.finite.html)

Other vector.na: [`isNA()`](isNA.md), [`locf()`](locf.md),
[`naIf()`](naIf.md), [`naReplace()`](naReplace.md)

## Examples

``` r

coalesceX(c(NA, NA, NA, 5, 3))
#> [1] 5
coalesceX(c(NA, NULL, "a"))
#> [1] "a"
coalesceX(NULL, 5, 3)
#> [1] 5

d.frm <- data.frame(matrix(c(
  1, 2, NA, 4,
  NA, NA, 3, 1,
  NaN, 2, 3, 1,
  NA, Inf, 1, 1), nrow=4, byrow=TRUE)
)

coalesceX(d.frm)
#> [1]   1   3   2 Inf
coalesceX(as.matrix(d.frm))
#> [1]   1   3   2 Inf
coalesceX(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4)
#> [1]   1   3   2 Inf
coalesceX(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4, method="is.finite")
#> [1] 1 3 2 1
coalesceX(list(d.frm[,1], d.frm[,2]))
#> [1]   1  NA   2 Inf

# returns the first finite element (skips NA, Inf, NaN)
coalesceX(d.frm, method="is.finite")
#> [1] 1 3 2 1

# returns the first argument that is not NULL
coalesceX(NULL, NULL, 7, method = "is.null")
#> [1] 7

# with characters (take care, factors won't work!)
# is.finite does not make sense here...
d.frm <- data.frame(matrix(c(
  "a", "b", NA, "4",
  NA, NA, "g", "m",
  NA_character_,"hfdg", "rr", "m",
  NA, Inf, 1, 1), nrow=4, byrow=TRUE)
, stringsAsFactors = FALSE)

coalesceX(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4)
#> [1] "a"    "g"    "hfdg" "Inf" 
coalesceX(d.frm)
#> [1] "a"    "g"    "hfdg" "Inf" 
coalesceX(as.list(d.frm))
#> [1] "a"    "g"    "hfdg" "Inf" 
```
