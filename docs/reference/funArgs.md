# List all arguments of a function

Returns the formal arguments of a function together with their default
values.

## Usage

``` r
funArgs(
  fun,
  package = NULL,
  sort = FALSE,
  output = c("data.frame", "list", "string")
)
```

## Arguments

- fun:

  Function object or function name.

- package:

  Optional package name used to resolve `fun`.

- sort:

  Logical; should arguments be sorted alphabetically? Ignored when
  `output = "list"`.

- output:

  Character string specifying the output format:

  - `"data.frame"` (default): return a data frame.

  - `"list"`: return a named list of formal arguments.

  - `"string"`: return a comma-separated character string of argument
    assignments.

## Value

Depending on `output`:

- `"data.frame"`: a data frame with columns `name` and `value`.

- `"list"`: a named list of formal arguments.

- `"string"`: a character vector of length one.

## See also

[`formals`](https://rdrr.io/r/base/formals.html),
[`args`](https://rdrr.io/r/base/args.html)

Other pkg.introspection: [`funCalls()`](funCalls.md),
[`funKeywords()`](funKeywords.md), [`funList()`](funList.md),
[`rdLabels()`](rdLabels.md), [`mergeArgs()`](mergeArgs.md)

## Examples

``` r
funArgs("combN")
#>      name value
#> n       n      
#> m       m      
#> repl repl FALSE
#> ord   ord FALSE

funArgs("combN", output = "list")
#> $n
#> 
#> 
#> $m
#> 
#> 
#> $repl
#> [1] FALSE
#> 
#> $ord
#> [1] FALSE
#> 

funArgs("combN", output = "string")
#> [1] "n, m, repl = FALSE, ord = FALSE"

cat(funArgs("combN", output = "string"))
#> n, m, repl = FALSE, ord = FALSE
```
