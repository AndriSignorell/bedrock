# Coerce `xtabs` Object to Array or Matrix

Converts an object of class `"xtabs"` to a plain array or matrix by
dropping all additional classes such as `"xtabs"` and `"table"`, along
with the `call` attribute.

## Usage

``` r
# S3 method for class 'xtabs'
as.array(x, ...)

# S3 method for class 'xtabs'
as.matrix(x, ...)
```

## Arguments

- x:

  an object of class `"xtabs"`

- ...:

  ignored

## Value

An array (or matrix in the two-dimensional case) with no additional
classes.

## See also

Other data.coerce: [`toBaseR()`](toBaseR.md),
[`type-aliases`](type-aliases.md)

## Examples

``` r
xt <- xtabs(~ cyl + gear, data = mtcars)
class(as.matrix(xt))
#> [1] "matrix" "array" 
# "matrix" "array"
```
