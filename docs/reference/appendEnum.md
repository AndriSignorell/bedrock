# Add an Enumeration Column

Prepends (or inserts) a column of enumeration labels – lowercase or
uppercase Roman numerals, or Arabic numbers.

## Usage

``` r
appendEnum(
  x,
  type = c("roman-lcase", "roman-ucase", "arabic"),
  suffix = ". ",
  startWith = 1L,
  after = 0L,
  colName = NULL
)
```

## Arguments

- x:

  a data.frame or matrix (vectors are coerced via
  [`matrix()`](https://rdrr.io/r/base/matrix.html))

- type:

  enumeration style: `"roman-lcase"`, `"roman-ucase"` or `"arabic"`

- suffix:

  text appended to each enumeration label

- startWith:

  first enumeration index

- after:

  position after which the column is inserted (see
  [`appendX`](appendX.md)); default `0L` prepends it

- colName:

  optional name for the new column; `NULL` (default) leaves it unnamed
  for matrices, while for data frames a default name (`"V1"`) is used

## Value

`x` with an additional enumeration column

## See also

[`append`](https://rdrr.io/r/base/append.html)

Other data.append: [`appendRowNames()`](appendRowNames.md),
[`appendX()`](appendX.md), [`multMerge()`](multMerge.md)

## Examples

``` r
d <- data.frame(x = 1:3, y = c("a", "b", "c"))
appendEnum(d)
#>      V1 x y
#> 1   i.  1 a
#> 2  ii.  2 b
#> 3 iii.  3 c

appendEnum(d, type = "arabic", suffix = ") ")
#>    V1 x y
#> 1 1)  1 a
#> 2 2)  2 b
#> 3 3)  3 c

# insert after the first column instead of prepending
appendEnum(d, after = 1L, colName = "no")
#>   x    no y
#> 1 1   i.  a
#> 2 2  ii.  b
#> 3 3 iii.  c
```
