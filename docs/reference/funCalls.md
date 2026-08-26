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

[`funList`](https://andrisignorell.github.io/bedrock/reference/funList.md),
[`funArgs`](https://andrisignorell.github.io/bedrock/reference/funArgs.md),
[`getParseData`](https://rdrr.io/r/utils/getParseData.html)

Other pkg.funinfo:
[`funArgs()`](https://andrisignorell.github.io/bedrock/reference/funArgs.md),
[`funKeywords()`](https://andrisignorell.github.io/bedrock/reference/funKeywords.md),
[`funList()`](https://andrisignorell.github.io/bedrock/reference/funList.md),
[`rdLabels()`](https://andrisignorell.github.io/bedrock/reference/rdLabels.md),
[`rdTitle()`](https://andrisignorell.github.io/bedrock/reference/rdTitle.md)

## Examples

``` r

funCalls("combN", package="bedrock")
#> named list()
```
