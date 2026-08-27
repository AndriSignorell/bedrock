# Merge Default Arguments with User Overrides

Helper used to merge defaults with user arguments, remove forbidden
argument names, and optionally warn if forbidden arguments were
supplied.

## Usage

``` r
mergeArgs(defaults, user, forbidden = NULL, warn = TRUE)
```

## Arguments

- defaults:

  named list of default arguments.

- user:

  named list of user-supplied arguments, or `NULL`.

- forbidden:

  character vector of argument names that are not allowed.

- warn:

  logical; whether to issue a warning if forbidden arguments are
  removed.

## Value

a named list of merged arguments.

## Details

User values override defaults of the same name. Unlike
[`modifyList`](https://rdrr.io/r/utils/modifyList.html), elements with
the value `NULL` are preserved (so that an explicit `NULL` can be passed
on as an argument value instead of silently deleting the entry).

## See also

[`utils::modifyList()`](https://rdrr.io/r/utils/modifyList.html)

Other pkg.args: [`callIf()`](callIf.md),
[`extractArgs()`](extractArgs.md), [`getDotsArg()`](getDotsArg.md),
[`recycle()`](recycle.md)

## Examples

``` r
mergeArgs(list(col = "black", lty = 1), list(col = "red"))
#> $col
#> [1] "red"
#> 
#> $lty
#> [1] 1
#> 

# explicit NULL survives the merge
mergeArgs(list(col = "black"), list(col = NULL))
#> $col
#> NULL
#> 
```
