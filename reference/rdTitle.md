# Extract the Title from an Rd Help File

Searches all `.Rd` files in a package's `man/` directory for a given
topic (matched against `\alias` entries) and returns its `\title`
string.

## Usage

``` r
rdTitle(topic, man = "man")
```

## Arguments

- topic:

  a single character string giving the topic (function name or alias) to
  look up.

- man:

  a single character string giving the path to the directory containing
  `.Rd` files. Defaults to `"man"`, i.e. the `man/` subdirectory of the
  current working directory.

## Value

a single character string with the title, trimmed of leading and
trailing whitespace. Stops with an error if `topic` is not found.

## See also

[`parse_Rd`](https://rdrr.io/r/tools/parse_Rd.html)

Other pkg.funinfo:
[`funArgs()`](https://andrisignorell.github.io/bedrock/reference/funArgs.md),
[`funCalls()`](https://andrisignorell.github.io/bedrock/reference/funCalls.md),
[`funKeywords()`](https://andrisignorell.github.io/bedrock/reference/funKeywords.md),
[`funList()`](https://andrisignorell.github.io/bedrock/reference/funList.md),
[`rdLabels()`](https://andrisignorell.github.io/bedrock/reference/rdLabels.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rdTitle("mean")
rdTitle("lm", man = "path/to/pkg/man")
} # }
```
