# List All Arguments of a Function

Returns the formal arguments of a function together with their default
values.

## Usage

``` r
funArgs(
  fun,
  package = NULL,
  sorted = FALSE,
  output = c("data.frame", "list", "string")
)
```

## Arguments

- fun:

  function object or function name.

- package:

  optional package name used to resolve `fun`.

- sorted:

  logical; should arguments be sorted alphabetically? `...` is always
  kept last. Ignored when `output = "list"`.

- output:

  character string specifying the output format:

  - `"data.frame"` (default): return a data frame.

  - `"list"`: return a named list of formal arguments.

  - `"string"`: return a comma-separated character string of argument
    assignments.

## Value

depending on `output`:

- `"data.frame"`: a data frame with columns `name` and `value`.

- `"list"`: a named list of formal arguments.

- `"string"`: a character vector of length one.

## See also

[`formals`](https://rdrr.io/r/base/formals.html),
[`args`](https://rdrr.io/r/base/args.html)

Other pkg.funinfo: [`funCalls()`](funCalls.md),
[`funKeywords()`](funKeywords.md), [`funList()`](funList.md),
[`rdLabels()`](rdLabels.md), [`rdTitle()`](rdTitle.md)

## Examples

``` r
funArgs("combN")
#>            name value
#> n             n      
#> m             m      
#> replace replace FALSE
#> ordered ordered FALSE

funArgs("combN", output = "list")
#> $n
#> 
#> 
#> $m
#> 
#> 
#> $replace
#> [1] FALSE
#> 
#> $ordered
#> [1] FALSE
#> 

funArgs("combN", output = "string")
#> [1] "n, m, replace = FALSE, ordered = FALSE"

cat(funArgs("combN", output = "string"))
#> n, m, replace = FALSE, ordered = FALSE
```
