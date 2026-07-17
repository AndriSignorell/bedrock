# Append Elements to Objects

Generic function to append elements to vectors, matrices, and data
frames.

## Usage

``` r
appendX(x, values, after = NULL, ...)

# Default S3 method
appendX(x, values, after = NULL, ...)

# S3 method for class 'matrix'
appendX(x, values, after = NULL, rows = FALSE, newNames = NULL, ...)

# S3 method for class 'data.frame'
appendX(x, values, after = NULL, rows = FALSE, newNames = NULL, ...)

# S3 method for class 'TOne'
appendX(x, values, after = NULL, rows = TRUE, newNames = NULL, ...)
```

## Arguments

- x:

  object to which values are appended.

- values:

  values to insert into `x`. For matrices with `rows = TRUE`, values are
  read row by row.

- after:

  position after which to insert. If `NULL`, values are appended at the
  end. Use `0` to prepend.

- ...:

  additional arguments.

- rows:

  logical; if TRUE, insert rows instead of columns. Ignored for vectors.
  Note that the method for `TOne` objects defaults to `rows = TRUE`, as
  appending rows is the typical use case there.

- newNames:

  optional names for the inserted elements: column names when inserting
  columns, row names when inserting rows. When inserting a column into a
  data.frame without giving `newNames`, default names (`"V1"`, `"V2"`,
  ...) are used.

## Value

object of the same class as `x`.

## See also

[`append`](https://rdrr.io/r/base/append.html)

Other data.append: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`multMerge()`](multMerge.md)

## Examples

``` r
# vectors
appendX(1:5, 99, after = 2)
#> [1]  1  2 99  3  4  5

# matrices: insert a column / a row
m <- matrix(1:6, nrow = 2,
            dimnames = list(c("r1", "r2"), c("a", "b", "c")))
appendX(m, c(9, 9), after = 1, newNames = "z")
#>    a z b c
#> r1 1 9 3 5
#> r2 2 9 4 6
appendX(m, 7:9, after = 1, rows = TRUE, newNames = "r1b")
#>     a b c
#> r1  1 3 5
#> r1b 7 8 9
#> r2  2 4 6

# data frames: insert a column / a row
d <- data.frame(a = 1:3, b = 4:6)
appendX(d, 7:9, after = 1, newNames = "z")
#>   a z b
#> 1 1 7 4
#> 2 2 8 5
#> 3 3 9 6
appendX(d, list(a = 99, b = 88), after = 0, rows = TRUE)
#>    a  b
#> 1 99 88
#> 2  1  4
#> 3  2  5
#> 4  3  6
```
