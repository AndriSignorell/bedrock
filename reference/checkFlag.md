# Validate a Logical Flag

Checks that an argument is a single non-missing `TRUE` or `FALSE`. Meant
for the many switches in the suite - `correct`, `unbiased`, `scaled`,
`paired` and the like - which were previously either unchecked or
checked in three different ways.

## Usage

``` r
checkFlag(x, name = deparse(substitute(x)))
```

## Arguments

- x:

  the value to check.

- name:

  the argument name to use in the message. Defaults to the expression
  that was passed, which is right in the ordinary case
  `checkFlag(correct)`; supply it explicitly when the caller passes
  something else, e.g. `checkFlag(args$correct, "correct")`.

## Value

`x`, invisibly.

## Details

`NA` is rejected on purpose. It is a logical of length one and therefore
passes [`is.logical()`](https://rdrr.io/r/base/logical.html), but a flag
that is neither on nor off has no meaning for a switch - and it
propagates silently, because `if (NA)` is an error somewhere further
down rather than here.

## See also

[checkConfLevel](https://andrisignorell.github.io/bedrock/reference/checkConfLevel.md)

## Examples

``` r
correct <- TRUE
checkFlag(correct)

if (FALSE) { # \dontrun{
correct <- NA
checkFlag(correct)             # "'correct' must be a single ..."
} # }
```
