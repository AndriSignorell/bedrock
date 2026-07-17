# Get a Single Argument from Dots with Default

Lightweight helper to extract a named argument from a list (typically
`list(...)`). If the argument is not present, a default value is
returned.

## Usage

``` r
getDotsArg(dots, name, default = NULL)
```

## Arguments

- dots:

  named list (usually `list(...)`).

- name:

  character string, argument name.

- default:

  default value if argument not present.

## Value

the value of the argument or `default`.

## See also

[`extractArgs`](extractArgs.md) for extracting several arguments at
once.

Other pkg.args: [`callIf()`](callIf.md),
[`extractArgs()`](extractArgs.md), [`mergeArgs()`](mergeArgs.md),
[`recycle()`](recycle.md)

## Examples

``` r
f <- function(...) {
  dots <- list(...)
  getDotsArg(dots, "col", default = "black")
}

f(col = "red", lwd = 2)
#> [1] "red"
f(lwd = 2)
#> [1] "black"
```
