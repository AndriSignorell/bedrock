# Operators To Check, If a Value Lies Within Or Outside a Given Range

The between and outside operators are used to check, whether a vector of
given values x lie within a defined range (or outside respectively). The
values can be numbers, text or dates. Ordered factors are supported.

## Usage

``` r
x %[]% rng

x %[)% rng

x %(]% rng

x %()% rng

x %][% rng

x %](% rng

x %)[% rng

x %)(% rng
```

## Arguments

- x:

  a variable with at least ordinal scale, usually a numeric value, but
  can be an ordered factor or a text as well. Texts would be treated
  alphabetically.

- rng:

  a vector of two values or a matrix with 2 columns, defining the
  minimum and maximum of the range for x.  
  If rng is a matrix, x or rng will be recycled. Matrix ranges are
  supported for numeric (and date) `x` only.

## Value

A logical vector of the same length as x.

## Details

The "BETWEEN" operators basically combine two conditional statements
into one and simplify the query process.  
They are merely a wrapper for: `\code{x >= rng[1] & x <= rng[2]}`, where
the round bracket `(` means *strictly greater (\>)* and the square
bracket `\code{[}` means *greater or equal (\>=)*. Numerical values of x
will be handled by C-code, which is significantly faster than two
comparisons in R (especially when x is huge).  

For the matching outside-operator, boundary elements of the
corresponding between-range return `FALSE`; that is, they are not
considered outside whenever the negated between-operator includes that
boundary.

Both arguments, `x` and `rng`, will be recycled to the highest
dimension, which is either the length of the vector (`x`) or the number
of rows of the matrix (`rng`).  
See also the routines used to check, whether two ranges overlap
([`overlap`](intervals.md), [`distance`](intervals.md)).

The "OUTSIDE" operators are the negations of the corresponding "BETWEEN"
operators, matched by *meaning* rather than by mirrored bracket symbols:

- `%][%` negates `%()%` (strictly outside both bounds)

- `%](%` negates `%(]%`

- `%)[%` negates `%[)%`

- `%)(%` negates `%[]%` (strictly outside, both bounds of the
  between-operator were closed)

## See also

[`if`](https://rdrr.io/r/base/Control.html),
[`ifelse`](https://rdrr.io/r/base/ifelse.html)

Other data.interval: [`intervals`](intervals.md),
[`range-operators`](range-operators.md)

## Examples

``` r

x <- 1:9
x %[]% c(3,5)
#> [1] FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE

# outside
x <- 1:9
x %][% c(3,5)
#> [1]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE

c(x,NA) %[]% c(3,5)
#>  [1] FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE    NA

x %(]% c(3,5)
#> [1] FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE

# no result when from > to:
x %[]% c(5,3)
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
x %(]% c(5,5)
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

# no problem:
ordered(x) %[]% c(3,5)
#> [1] FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE

# not meaningful:
factor(x) %[]% c(3,5)
#> [1] NA NA NA NA NA NA NA NA NA

# characters
letters[letters %(]% c("d","h")]
#> [1] "e" "f" "g" "h"

# select numbers between 0.4 and 0.5
x <- runif(20)
x %[]% c(0.4, 0.5)
#>  [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [13]  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE

# use it with an ordered factor
x <- ordered(sample(LETTERS, 20), levels = LETTERS)
x %[)% c("G","K")
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE
#> [13] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE


# use multiple ranges
2 %[]% cbind(1:4,2:5)
#> [1]  TRUE  TRUE FALSE FALSE

# both arguments are recycled
c(2,3) %[]% cbind(1:4,2:5)
#> [1]  TRUE  TRUE FALSE FALSE
```
