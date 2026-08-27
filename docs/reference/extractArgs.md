# Extract Named Arguments from Dots with Defaults

Utility to extract a subset of arguments from a list (typically
`list(...)`) and merge them with default values. Elements of `dots`
override entries in `defaults` of the same name; explicit `NULL` values
are preserved.

## Usage

``` r
extractArgs(dots, defaults, validate = NULL, returnRest = FALSE)
```

## Arguments

- dots:

  named list of arguments (usually `list(...)`).

- defaults:

  named list of default values.

- validate:

  optional validation function, called with the merged argument list for
  its side effect. It should throw an error on invalid input; its return
  value is ignored.

- returnRest:

  logical; if `TRUE`, a list with components `args` (the merged
  arguments) and `rest` (all elements of `dots` not matching a default,
  including unnamed ones) is returned.

## Value

named list of extracted arguments, or a list with components `args` and
`rest` if `returnRest = TRUE`.

## See also

Other pkg.args: [`callIf()`](callIf.md),
[`getDotsArg()`](getDotsArg.md), [`mergeArgs()`](mergeArgs.md),
[`recycle()`](recycle.md)

## Examples

``` r
dots <- list(col = "red", lwd = 2, 99)
extractArgs(dots, defaults = list(col = "black", lty = 1))
#> $col
#> [1] "red"
#> 
#> $lty
#> [1] 1
#> 

extractArgs(dots, defaults = list(col = "black", lty = 1),
            returnRest = TRUE)
#> $args
#> $args$col
#> [1] "red"
#> 
#> $args$lty
#> [1] 1
#> 
#> 
#> $rest
#> $rest$lwd
#> [1] 2
#> 
#> $rest[[2]]
#> [1] 99
#> 
#> 
```
