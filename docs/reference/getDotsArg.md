# Get a single argument from dots with default

Lightweight helper to extract a named argument from a list (typically
list(...)).

## Usage

``` r
getDotsArg(dots, name, default = NULL)
```

## Arguments

- dots:

  Named list (usually list(...))

- name:

  Character string, argument name

- default:

  Default value if argument not present

## Value

The value of the argument or default

## See also

Other pkg.introspection: [`extractArgs()`](extractArgs.md),
[`funArgs()`](funArgs.md), [`funCalls()`](funCalls.md),
[`funKeywords()`](funKeywords.md), [`funList()`](funList.md),
[`mergeArgs()`](mergeArgs.md), [`quot()`](quot.md),
[`rdLabels()`](rdLabels.md), [`rdTitle()`](rdTitle.md),
[`strX()`](strX.md)
