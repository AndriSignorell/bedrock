# List Calls Used in Function

For screening purposes it can be useful to get a list of all function
calls our function may depend on. `funCalls()` parses the function
source and return all found function calls grouped by their package.

## Usage

``` r
funCalls(name, package = NULL, sort = FALSE)
```

## Arguments

- name:

  the name of the function

- package:

  the name of the package

- sort:

  logical (default `FALSE`) should the arguments be alphabetically
  sorted?

## Note

Based on code by Nicholas Cooper.

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

## See also

[`ls`](https://rdrr.io/r/base/ls.html),
[`ls.str`](https://rdrr.io/r/utils/ls_str.html),
[`lsf.str`](https://rdrr.io/r/utils/ls_str.html)

Other pkg.introspection: [`funArgs()`](funArgs.md),
[`funKeywords()`](funKeywords.md), [`funList()`](funList.md),
[`getRdLabels()`](getRdLabels.md), [`mergeArgs()`](mergeArgs.md)

## Examples

``` r

funCalls("combN", package="bedrock")
#> named list()
```
