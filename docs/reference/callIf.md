# Conditionally Call a Function

Conditionally evaluate a function depending on the value of an argument.
This is a convenient helper for optional features such as plotting,
logging, or callbacks, where the user can enable, disable, or
parameterize a function call via a single argument.

## Usage

``` r
callIf(fun, arg, defaults = NULL, forbidden = NULL, warn = TRUE)
```

## Arguments

- fun:

  a function to be called

- arg:

  controls whether and how `fun` is called:

  - `FALSE`, `NULL`, or `NA`: `fun` is not called and `NULL` is returned
    invisibly.

  - `TRUE`: `fun` is called with `defaults` (if provided), or with no
    arguments.

  - A fully named list: `fun` is called with the list elements as
    arguments. If `defaults` is provided, it is merged with `arg`, where
    elements of `arg` override those in `defaults`.

- defaults:

  a named list of default arguments passed to `fun` when `arg = TRUE`,
  or used as a base when `arg` is a list. Default is `NULL`.

- forbidden:

  optional character vector of argument names that are not allowed. If
  any of these appear in `arg`, they are removed before calling `fun`. A
  warning is issued unless `warn = FALSE`.

- warn:

  logical. If `TRUE` (default), a warning is issued when forbidden
  arguments are removed.

## Value

Returns the result of `fun(...)` if called. If `arg` is `FALSE`, `NULL`,
or `NA`, returns `NULL` invisibly.

## Details

This function implements a flexible pattern for optional function calls:

- Enable/disable behavior with `TRUE`/`FALSE`

- Customize behavior with a list of arguments

- Provide safe defaults and restrict certain arguments

When merging `defaults` and `arg`, user-supplied arguments take
precedence. Unlike
[`modifyList`](https://rdrr.io/r/utils/modifyList.html), elements with
the value `NULL` are preserved and passed on to `fun` (so that an
explicit `NULL` can be used to reset an argument).

## See also

Other pkg.args: [`extractArgs()`](extractArgs.md),
[`getDotsArg()`](getDotsArg.md), [`mergeArgs()`](mergeArgs.md),
[`recycle()`](recycle.md)

## Examples

``` r
# Simple usage: skip
callIf(message, FALSE)

# Call with defaults
callIf(message, TRUE, defaults = list("Hello world"))
#> Hello world

# Call with explicit arguments
callIf(message, list(x = "Hello from callIf"))
#> Hello from callIf

# With defaults + override
callIf(plot, list(x = 1:5),
       defaults = list(y = 1:5, type = "l"))


# Forbid arguments
callIf(plot,
       list(x = 1:5, y = 1:5, col = "red"),
       forbidden = "col")
#> Warning: Ignoring forbidden argument(s) for 'plot': col


# Typical use case: optional plotting
x <- 1:10
y <- x^2
callIf(plot, TRUE, defaults = list(x, y))

```
