# List Calls Used in Function

For screening purposes it can be useful to get a list of all function
calls our function may depend on. `funCalls()` parses the function
source and returns all found function calls grouped by their package.

## Usage

``` r
funCalls(name, package = NULL, sorted = FALSE)
```

## Arguments

- name:

  the name of the function.

- package:

  optional name of a package; if given, the result is filtered to source
  environments matching `package`.

- sorted:

  logical; whether calls are sorted alphabetically. Defaults to `FALSE`.

## Value

a list of character vectors with the function calls, grouped by the
environment the called functions were found in.

## Details

The source packages are resolved via
[`find`](https://rdrr.io/r/utils/apropos.html), which only sees attached
packages. Calls to functions from packages that are not on the search
path are reported under `"<not found>"`.

## Note

Based on code by Nicholas Cooper.

## See also

[`funList`](funList.md), [`funArgs`](funArgs.md),
[`getParseData`](https://rdrr.io/r/utils/getParseData.html)

Other pkg.funinfo: [`funArgs()`](funArgs.md),
[`funKeywords()`](funKeywords.md), [`funList()`](funList.md),
[`rdLabels()`](rdLabels.md), [`rdTitle()`](rdTitle.md)

## Examples

``` r

funCalls("combN", package="bedrock")
#> named list()
```
