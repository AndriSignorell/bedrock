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

  target length

- fill:

  value used for newly added elements when `x` is extended (default
  `""`)

## Value

`x`, of length `n`

## See also

Other vector.ops: [`closest()`](closest.md),
[`coalesceX()`](coalesceX.md), [`locf()`](locf.md), [`midx()`](midx.md),
[`moveAvg()`](moveAvg.md), [`naIf()`](naIf.md),
[`naReplace()`](naReplace.md), [`nz()`](nz.md),
[`pairApply()`](pairApply.md), [`trim()`](trim.md), [`vRot()`](vRot.md),
[`vShift()`](vShift.md), [`winsorize()`](winsorize.md)

## Examples

``` r
setLength(LETTERS[1:3], 5)
#> [1] "A" "B" "C" NA  NA 
setLength(LETTERS[1:3], 2)
#> [1] "A" "B"
setLength(1:4, 6, fill = 0)
#> [1] 1 2 3 4 0 0
```
