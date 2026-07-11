# Set the length of a vector, padding or truncating as needed

Extends `x` to length `n` by appending `fill`, or truncates it to length
`n` if it is already longer – like `length(x) <- n`, but with a
configurable fill value instead of `NA`.

## Usage

``` r
setLength(x, n, fill = NA)
```

## Arguments

- x:

  a vector

- n:

  target length, a single non-negative whole number

- fill:

  value used for newly added elements when `x` is extended (default
  `NA`)

## Value

`x`, of length `n`

## See also

Other vector.reshape: [`trim()`](trim.md), [`vRot()`](vRot.md),
[`vShift()`](vShift.md)

## Examples

``` r
setLength(LETTERS[1:3], 5)
#> [1] "A" "B" "C" NA  NA 
setLength(LETTERS[1:3], 2)
#> [1] "A" "B"
setLength(1:4, 6, fill = 0)
#> [1] 1 2 3 4 0 0
```
