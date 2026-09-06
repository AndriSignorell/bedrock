# Reverse the Order of Elements

Returns a reversed version of its argument. Where
[`rev()`](https://rdrr.io/r/base/rev.html) treats every object as one
long vector, `revX()` reverses the order along the dimensions of a
multidimensional object, so that a matrix, table, array or data frame
comes back with its rows, its columns, or both, in the opposite order
and its `dimnames` moved along with the data. Which dimensions are
turned around is chosen with `margin`.

## Usage

``` r
revX(x, ...)

# Default S3 method
revX(x, margin = 1L, ...)

# S3 method for class 'array'
revX(x, margin = seq_along(dim(x)), ...)

# S3 method for class 'matrix'
revX(x, margin = seq_along(dim(x)), ...)

# S3 method for class 'table'
revX(x, margin = seq_along(dim(x)), ...)

# S3 method for class 'data.frame'
revX(x, margin = 1:2, ...)
```

## Arguments

- x:

  a vector, matrix, table, array or data frame to be reversed.

- ...:

  further arguments, passed on to the method dispatched on. This is how
  `margin` is handed over; arguments beyond it are ignored with a
  warning.

- margin:

  the dimensions to reverse, `1` for the rows, `2` for the columns, and
  so on, each at most once. Defaults to all dimensions of `x`, which for
  a vector means `1`.

## Value

an object of the same class and dimensions as `x`, with the order of the
elements along `margin` reversed.

## Details

A vector has one dimension and is simply reversed, as by
[`rev()`](https://rdrr.io/r/base/rev.html), with `margin = 1` accepted
so that calling code need not know whether its argument has dimensions.
For everything else, `margin` names the dimensions to be reversed, `1`
for the rows, `2` for the columns, and so on for the higher dimensions
of an array; the default reverses all of them. The values in the object
are not rearranged relative to their labels: reversing an object twice
along the same margin returns the original.

`margin` names each dimension at most once; repeating one says nothing
and is refused, as is any value outside the dimensions of `x`.

The additional arguments of the generic are the way `margin` reaches the
methods. Anything else is ignored with a warning, rather than being
dropped silently although it was meant to change the result.

## See also

[`rev()`](https://rdrr.io/r/base/rev.html),
[`order()`](https://rdrr.io/r/base/order.html),
[`sort()`](https://rdrr.io/r/base/sort.html),
[`seq()`](https://rdrr.io/r/base/seq.html)

Other data.order: [`binaryTree()`](binaryTree.md), [`sortX()`](sortX.md)

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

# the dimnames travel with the data, so this is not a transposition
revX(tab, margin=c(1, 2))["3", "a"] == tab["3", "a"]
#> [1] TRUE
## [1] TRUE

# reverse a 3-dimensional array
aa <- array(c(tab, 2 * tab), dim = c(3, 3, 2),
            dimnames = c(dimnames(tab), list(mar3 = c("A", "Z"))))

# reverse rows
revX(aa, 1)
#> , , mar3 = A
#> 
#>     mar2
#> mar1 a  b   c
#>    3 3 33 333
#>    2 2 22 222
#>    1 1 11 111
#> 
#> , , mar3 = Z
#> 
#>     mar2
#> mar1 a  b   c
#>    3 6 66 666
#>    2 4 44 444
#>    1 2 22 222
#> 
# reverse columns
revX(aa, 2)
#> , , mar3 = A
#> 
#>     mar2
#> mar1   c  b a
#>    1 111 11 1
#>    2 222 22 2
#>    3 333 33 3
#> 
#> , , mar3 = Z
#> 
#>     mar2
#> mar1   c  b a
#>    1 222 22 2
#>    2 444 44 4
#>    3 666 66 6
#> 
# reverse the third dimension
revX(aa, 3)
#> , , mar3 = Z
#> 
#>     mar2
#> mar1 a  b   c
#>    1 2 22 222
#>    2 4 44 444
#>    3 6 66 666
#> 
#> , , mar3 = A
#> 
#>     mar2
#> mar1 a  b   c
#>    1 1 11 111
#>    2 2 22 222
#>    3 3 33 333
#> 

# reverse all dimensions
revX(aa)
#> , , mar3 = Z
#> 
#>     mar2
#> mar1   c  b a
#>    3 666 66 6
#>    2 444 44 4
#>    1 222 22 2
#> 
#> , , mar3 = A
#> 
#>     mar2
#> mar1   c  b a
#>    3 333 33 3
#>    2 222 22 2
#>    1 111 11 1
#> 
# same as
revX(aa, margin = 1:3)
#> , , mar3 = Z
#> 
#>     mar2
#> mar1   c  b a
#>    3 666 66 6
#>    2 444 44 4
#>    1 222 22 2
#> 
#> , , mar3 = A
#> 
#>     mar2
#> mar1   c  b a
#>    3 333 33 3
#>    2 222 22 2
#>    1 111 11 1
#> 

# data frames are reversed by rows, by columns or both
d <- data.frame(a = 1:3, b = 4:6)
revX(d, 1)
#>   a b
#> 3 3 6
#> 2 2 5
#> 1 1 4
revX(d, 2)
#>   b a
#> 1 4 1
#> 2 5 2
#> 3 6 3
```
