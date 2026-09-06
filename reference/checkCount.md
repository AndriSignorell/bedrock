# Validate a Count

Checks that an argument is a single finite integer, not smaller than
`min`. Meant for the many size arguments in the suite - `digits`, `sep`,
`width`, `nPerm`, `R` and the like - which are conceptually counts
rather than numbers and were previously spelled out by hand wherever
they occur.

## Usage

``` r
checkCount(x, min = 0L, name = deparse(substitute(x)))
```

## Arguments

- x:

  the value to check.

- min:

  the smallest admissible value, `0` by default. Pass `1` for the
  arguments that must be positive, e.g. a width or a number of
  replicates.

- name:

  the argument name to use in the message. Defaults to the expression
  that was passed.

## Value

`x`, invisibly.

## Details

A whole number stored as a double is accepted, as that is what
arithmetic on integers produces and what a user typing `2` supplies.
`TRUE` is not, although it would survive
[`as.integer()`](https://rdrr.io/r/base/integer.html): a flag that
reaches a count argument is a mistake, not a shorthand for one.

The order of the tests is the same as in
[`checkConfLevel()`](checkConfLevel.md), length first, then type, then
value, so that the message names the argument rather than the condition
that failed.

## See also

[`checkConfLevel()`](checkConfLevel.md), [`checkFlag()`](checkFlag.md),
[`checkString()`](checkString.md)

## Examples

``` r
sep <- 2
checkCount(sep)

width <- 80
checkCount(width, min = 1)

if (FALSE) { # \dontrun{
checkCount(1.5)                # not a whole number
checkCount(-1)                 # below the default minimum
checkCount(TRUE)               # a flag is not a count
} # }
```
