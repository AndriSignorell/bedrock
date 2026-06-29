# Merge default arguments with user overrides

Internal helper used to:

- merge defaults with user arguments

- remove forbidden argument names

- optionally warn if forbidden arguments were supplied

## Usage

``` r
mergeArgs(defaults, user, forbidden = NULL, warn = TRUE)
```

## Arguments

- defaults:

  Named list of default arguments.

- user:

  Named list of user-supplied arguments.

- forbidden:

  Character vector of argument names that are not allowed.

- warn:

  Logical; whether to issue a warning if forbidden arguments are
  removed.

## Value

A named list of merged arguments.

## See also

Other pkg.introspection: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funKeywords()`](funKeywords.md),
[`funList()`](funList.md), [`rdLabels()`](rdLabels.md)
