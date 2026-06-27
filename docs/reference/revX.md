# Reverse Elements of a Vector, a Matrix, a Table, an Array or a Data.frame

`revX` provides a reversed version of its argument. Unlike the basic
function, it does in higher-dimensional structures such as matrices not
reverse the elements, but the order of the rows and/or columns. It
further offers additional interfaces for higher dimensional arrays or
tables.

## Usage

``` r
revX(x, ...)

# Default S3 method
revX(x, ...)

# S3 method for class 'array'
revX(x, margin, ...)

# S3 method for class 'matrix'
revX(x, margin, ...)

# S3 method for class 'table'
revX(x, margin, ...)

# S3 method for class 'data.frame'
revX(x, margin, ...)
```

## Arguments

- x:

  a vector, a matrix or a higher dimensional table to be reversed.

- ...:

  the dots are passed to the array interface.

- margin:

  vector of dimensions which to be reversed (1 for rows, 2 for columns,
  etc.). If not defined, all dimensions will be reverted.

## See also

[`rev`](https://rdrr.io/r/base/rev.html),
[`order`](https://rdrr.io/r/base/order.html),
[`sort`](https://rdrr.io/r/base/sort.html),
[`seq`](https://rdrr.io/r/base/seq.html)

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md),
[`columnWrap()`](columnWrap.md), [`combLevels()`](combLevels.md),
[`nf()`](nf.md), [`parseSASDatalines()`](parseSASDatalines.md),
[`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`revCode()`](revCode.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`splitX()`](splitX.md), [`stringsAsFactors()`](stringsAsFactors.md),
[`toBaseR()`](toBaseR.md), [`untable()`](untable.md)

## Examples

``` r

tab <- matrix(c(1, 11, 111,
                2, 22, 222,
                3, 33, 333), 
              byrow=TRUE, nrow=3,
              dimnames=list(mar1=1:3, mar2=c("a","b","c")))

revX(tab, margin=1)
#>     mar2
#> mar1 a  b   c
#>    3 3 33 333
#>    2 2 22 222
#>    1 1 11 111
revX(tab, margin=2)
#>     mar2
#> mar1   c  b a
#>    1 111 11 1
#>    2 222 22 2
#>    3 333 33 3

# reverse both dimensions
revX(tab, margin=c(1, 2))
#>     mar2
#> mar1   c  b a
#>    3 333 33 3
#>    2 222 22 2
#>    1 111 11 1

t(tab)
#>     mar1
#> mar2   1   2   3
#>    a   1   2   3
#>    b  11  22  33
#>    c 111 222 333

# reverse 3dimensional array
aa <- abind(tab, 2 * tab, along=3)
dimnames(aa)[[3]] <- c("A","Z")

# reverse rows
revX(aa, 1)
#> , , A
#> 
#>   a  b   c
#> 3 3 33 333
#> 2 2 22 222
#> 1 1 11 111
#> 
#> , , Z
#> 
#>   a  b   c
#> 3 6 66 666
#> 2 4 44 444
#> 1 2 22 222
#> 
# reverse columns
revX(aa, 2)
#> , , A
#> 
#>     c  b a
#> 1 111 11 1
#> 2 222 22 2
#> 3 333 33 3
#> 
#> , , Z
#> 
#>     c  b a
#> 1 222 22 2
#> 2 444 44 4
#> 3 666 66 6
#> 
# reverse 3th dimension
revX(aa, 3)
#> , , Z
#> 
#>   a  b   c
#> 1 2 22 222
#> 2 4 44 444
#> 3 6 66 666
#> 
#> , , A
#> 
#>   a  b   c
#> 1 1 11 111
#> 2 2 22 222
#> 3 3 33 333
#> 

# reverse all dimensions
revX(aa)
#> , , Z
#> 
#>     c  b a
#> 3 666 66 6
#> 2 444 44 4
#> 1 222 22 2
#> 
#> , , A
#> 
#>     c  b a
#> 3 333 33 3
#> 2 222 22 2
#> 1 111 11 1
#> 
# same as
revX(aa, margin=(1:3))
#> , , Z
#> 
#>     c  b a
#> 3 666 66 6
#> 2 444 44 4
#> 1 222 22 2
#> 
#> , , A
#> 
#>     c  b a
#> 3 333 33 3
#> 2 222 22 2
#> 1 111 11 1
#> 
```
