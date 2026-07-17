# Type Coercion Shortcuts

Concise aliases for common base R coercion functions. `num()`, `int()`,
`chr()` are direct wrappers around
[`as.numeric()`](https://rdrr.io/r/base/numeric.html),
[`as.integer()`](https://rdrr.io/r/base/integer.html), and
[`as.character()`](https://rdrr.io/r/base/character.html). `nchr()`
handles the common pitfall of coercing factors to numeric. `bin()`
converts any two-valued vector to logical.

## Usage

``` r
num(x, ...)

int(x, ...)

chr(x, ...)

nchr(x)

bin(x, ...)
```

## Arguments

- x:

  a vector. For `bin()`, exactly two unique non-`NA` values are
  required.

- ...:

  further arguments passed to the underlying base function
  (`as.numeric`, `as.integer`, `as.character`, or
  [`asBinary`](asBinary.md)).

## Value

a vector of the target type and the same length as `x`.

## Details

- `num(x, ...)`:

  Equivalent to `as.numeric(x)`.

- `int(x, ...)`:

  Equivalent to `as.integer(x)`.

- `chr(x, ...)`:

  Equivalent to `as.character(x)`.

- `nchr(x)`:

  Shortcut for `as.numeric(as.character(x))`. Avoids the trap of
  `as.numeric(factor)` returning internal integer codes instead of the
  label values.

- `bin(x, ...)`:

  Converts a two-valued vector (character, factor, integer, or numeric)
  to logical. Mapping follows
  [`factor()`](https://rdrr.io/r/base/factor.html) level order: the
  *first* level becomes `FALSE`, the *second* `TRUE`. To reverse, use
  `!bin(x)`.

## See also

[`nf()`](nf.md), [`asBinary()`](asBinary.md)

Other data.coerce: [`as.array.xtabs()`](as.array.xtabs.md),
[`toBaseR()`](toBaseR.md)

## Examples

``` r
num("3.14")
#> [1] 3.14
int(3.9)                               # truncates, does not round
#> [1] 3
chr(1:3)
#> [1] "1" "2" "3"
nchr(factor(c("1.5", "2.0", "1.5"))) # correct: 1.5 2.0 1.5
#> [1] 1.5 2.0 1.5
as.numeric(factor(c("1.5", "2.0")))  # wrong:   1   2
#> [1] 1 2

bin(c(0L, 1L, 0L, 1L))
#> [1] FALSE  TRUE FALSE  TRUE
bin(c("no", "yes", "no"))            # "no" -> FALSE, "yes" -> TRUE
#> [1] FALSE  TRUE FALSE
#> attr(,"coding")
#>  no yes 
#>   0   1 
!bin(c("no", "yes", "no"))           # reversed
#> [1]  TRUE FALSE  TRUE
#> attr(,"coding")
#>  no yes 
#>   0   1 
bin(factor(c("m", "w", "m")))        # "m" -> FALSE, "w" -> TRUE
#> [1] FALSE  TRUE FALSE
#> attr(,"coding")
#> m w 
#> 0 1 
```
