# Recover Original Data From Contingency Table

Recreates the data.frame out of a contingency table x.

## Usage

``` r
untable(x, ...)

# S3 method for class 'data.frame'
untable(x, freq = "Freq", rownames = NULL, ...)

# Default S3 method
untable(x, dimnames = NULL, type = NULL, rownames = NULL, colnames = NULL, ...)
```

## Arguments

- x:

  a numeric vector, a matrix, a table or a data.frame. If x is a vector,
  a matrix or a table it is interpreted as frequencies which are to be
  inflated to the original list.  
  If x is a data.frame it is interpreted as a table in frequency form
  (containing one or more factors and a frequency variable).

- ...:

  further arguments passed to or from functions (not used here)

- freq:

  character, the name of the frequency variable in case x is a
  data.frame.

- rownames:

  a names vector for the rownames of the resulting data.frame If set to
  `NULL` (default) the names will be defined according to the table's
  dimnames.

- dimnames:

  the dimension names of x to be used for expanding. Can be used to
  expand a weight vector to its original values. If set to `NULL`
  (default) the dimnames of x will be used.

- type:

  defines the data type generated. This allows to directly define
  factors or ordered factors, but also numeric values. See examples.

- colnames:

  a names vector for the colnames of the resulting data.frame If set to
  `NULL` (default) the names will be defined according to the table's
  dimnames.

## Value

a data.frame with the detailed data (even if x was a 1-dimensional
table)

## Details

For x being a vector this reduces to `rep(..., n)` with n as vector
(which is not supported by [`rep()`](https://rdrr.io/r/base/rep.html)).
`NA`s in the table will be treated as 0 without raising an error.

## See also

[`expand.grid`](https://rdrr.io/r/base/expand.grid.html),
[`rep`](https://rdrr.io/r/base/rep.html),
[`gl`](https://rdrr.io/r/base/gl.html),
[`xtabs`](https://rdrr.io/r/stats/xtabs.html)

Other data.reshape: [`collapseTable()`](collapseTable.md),
[`long-wide-reshape`](long-wide-reshape.md), [`splitAt()`](splitAt.md),
[`splitX()`](splitX.md)

## Examples

``` r

d.titanic <- untable(Titanic)
str(d.titanic)
#> 'data.frame':    2201 obs. of  4 variables:
#>  $ Class   : Factor w/ 4 levels "1st","2nd","3rd",..: 3 3 3 3 3 3 3 3 3 3 ...
#>  $ Sex     : Factor w/ 2 levels "Male","Female": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ Age     : Factor w/ 2 levels "Child","Adult": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ Survived: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
#>  - attr(*, "out.attrs")=List of 2
#>   ..$ dim     : Named int [1:4] 4 2 2 2
#>   .. ..- attr(*, "names")= chr [1:4] "Class" "Sex" "Age" "Survived"
#>   ..$ dimnames:List of 4
#>   .. ..$ Class   : chr [1:4] "Class=1st" "Class=2nd" "Class=3rd" "Class=Crew"
#>   .. ..$ Sex     : chr [1:2] "Sex=Male" "Sex=Female"
#>   .. ..$ Age     : chr [1:2] "Age=Child" "Age=Adult"
#>   .. ..$ Survived: chr [1:2] "Survived=No" "Survived=Yes"

# ... not the same as:
data.frame(Titanic)
#>    Class    Sex   Age Survived Freq
#> 1    1st   Male Child       No    0
#> 2    2nd   Male Child       No    0
#> 3    3rd   Male Child       No   35
#> 4   Crew   Male Child       No    0
#> 5    1st Female Child       No    0
#> 6    2nd Female Child       No    0
#> 7    3rd Female Child       No   17
#> 8   Crew Female Child       No    0
#> 9    1st   Male Adult       No  118
#> 10   2nd   Male Adult       No  154
#> 11   3rd   Male Adult       No  387
#> 12  Crew   Male Adult       No  670
#> 13   1st Female Adult       No    4
#> 14   2nd Female Adult       No   13
#> 15   3rd Female Adult       No   89
#> 16  Crew Female Adult       No    3
#> 17   1st   Male Child      Yes    5
#> 18   2nd   Male Child      Yes   11
#> 19   3rd   Male Child      Yes   13
#> 20  Crew   Male Child      Yes    0
#> 21   1st Female Child      Yes    1
#> 22   2nd Female Child      Yes   13
#> 23   3rd Female Child      Yes   14
#> 24  Crew Female Child      Yes    0
#> 25   1st   Male Adult      Yes   57
#> 26   2nd   Male Adult      Yes   14
#> 27   3rd   Male Adult      Yes   75
#> 28  Crew   Male Adult      Yes  192
#> 29   1st Female Adult      Yes  140
#> 30   2nd Female Adult      Yes   80
#> 31   3rd Female Adult      Yes   76
#> 32  Crew Female Adult      Yes   20


tab <- table(set1=sample(letters[1:5], size=40, replace=TRUE), 
             set2=sample(letters[11:15], size=40, replace=TRUE))
untable(tab)
#>    set1 set2
#> 1     a    k
#> 2     c    k
#> 3     c    k
#> 4     d    k
#> 5     d    k
#> 6     d    k
#> 7     e    k
#> 8     e    k
#> 9     a    l
#> 10    a    l
#> 11    a    l
#> 12    b    l
#> 13    b    l
#> 14    d    l
#> 15    e    l
#> 16    a    m
#> 17    a    m
#> 18    a    m
#> 19    b    m
#> 20    c    m
#> 21    c    m
#> 22    c    m
#> 23    e    m
#> 24    a    n
#> 25    b    n
#> 26    b    n
#> 27    c    n
#> 28    c    n
#> 29    c    n
#> 30    c    n
#> 31    d    n
#> 32    a    o
#> 33    a    o
#> 34    b    o
#> 35    b    o
#> 36    b    o
#> 37    c    o
#> 38    c    o
#> 39    e    o
#> 40    e    o


# return a numeric vector by setting type and coerce to a vector by [,]
untable(c(6,2,2), type="as.numeric")[,]
#>  [1] 1 1 1 1 1 1 2 2 3 3


# how to produce the original list based on frequencies, given as a data.frame
d.freq <- data.frame(xtabs(Freq ~ Sex + Survived, data=Titanic))

# a data list with each individual
d.data <- untable( xtabs(c(1364, 126, 367, 344) ~ ., 
             expand.grid(levels(d.freq$Sex),levels(d.freq$Survived)))) 
head(d.data)
#>   Var1 Var2
#> 1 Male   No
#> 2 Male   No
#> 3 Male   No
#> 4 Male   No
#> 5 Male   No
#> 6 Male   No

# expand a weights vector
untable(c(1,4,5), dimnames=list(c("Zurich","Berlin","London")))
#>      Var1
#> 1  Zurich
#> 2  Berlin
#> 3  Berlin
#> 4  Berlin
#> 5  Berlin
#> 6  London
#> 7  London
#> 8  London
#> 9  London
#> 10 London

# and the same with a numeric vector 
untable(c(1,4,5), dimnames=list(c(5,10,15)), type="as.numeric")[,]
#>  [1]  5 10 10 10 10 15 15 15 15 15
# ... which again is nothing else than
rep(times=c(1,4,5), x=c(5,10,15))
#>  [1]  5 10 10 10 10 15 15 15 15 15

# the data.frame interface
d.freq <- data.frame(f1=c("A","A","B","B"), f2=c("C","D","C","D"), Freq=c(1,2,3,4))
untable(d.freq)
#>    f1 f2
#> 1   A  C
#> 2   A  D
#> 3   A  D
#> 4   B  C
#> 5   B  C
#> 6   B  C
#> 7   B  D
#> 8   B  D
#> 9   B  D
#> 10  B  D
```
