# Test Whether Multiple Objects Are Identical

Extends [`identical`](https://rdrr.io/r/base/identical.html) to more
than two objects. Returns `TRUE` if all supplied objects are exactly
identical, and `FALSE` otherwise.

## Usage

``` r
allIdentical(...)
```

## Arguments

- ...:

  objects to compare.

## Value

logical scalar.

## Details

If zero or one object is supplied, the function returns `TRUE`.

Note that the objects themselves are compared, not their elements. So
`allIdentical(list(A, B, C))` is `TRUE`, as a single object is trivially
identical to itself. Use `do.call(allIdentical, myList)` to compare the
elements of a list.

## See also

[`identical`](https://rdrr.io/r/base/identical.html)

Other data.equal: [`allDuplicated()`](allDuplicated.md),
[`compareDataFrames()`](compareDataFrames.md)

## Examples

``` r
A <- LETTERS[1:5]
B <- LETTERS[1:5]
C <- LETTERS[1:5]
E <- factor(LETTERS[1:5])

allIdentical(A, B, C)        # TRUE
#> [1] TRUE
allIdentical(A, B, C, E)     # FALSE
#> [1] FALSE

allIdentical(1, 1L)          # FALSE (type matters)
#> [1] FALSE

```
