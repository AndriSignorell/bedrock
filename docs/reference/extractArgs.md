# Extract named arguments from dots with defaults

Utility to extract a subset of arguments from a list (typically
`list(...)`) and merge them with default values.

## Usage

``` r
extractArgs(dots, defaults, validate = NULL, returnRest = FALSE)
```

## Arguments

- dots:

  Named list of arguments (usually `list(...)`)

- defaults:

  Named list of default values

- validate:

  Optional validation function

- returnRest:

  Logical; return unused arguments

## Value

Named list of extracted arguments (and optionally remaining ones)

## See also

Other pkg.introspection: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funKeywords()`](funKeywords.md),
[`funList()`](funList.md), [`getDotsArg()`](getDotsArg.md),
[`mergeArgs()`](mergeArgs.md), [`quot()`](quot.md),
[`rdLabels()`](rdLabels.md), [`rdTitle()`](rdTitle.md),
[`strX()`](strX.md)
