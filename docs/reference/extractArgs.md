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
