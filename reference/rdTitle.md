# Extract the Title from an Rd Help File

Searches all `.Rd` files in a package's `man/` directory for a given
topic (matched against `\\alias` entries) and returns its `\\title`
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

[`tools::parse_Rd()`](https://rdrr.io/r/tools/parse_Rd.html)

Other pkg.funinfo: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funKeywords()`](funKeywords.md),
[`funList()`](funList.md), [`rdLabels()`](rdLabels.md)

## Examples

``` r
# a minimal man/ directory to search in
man <- file.path(tempdir(), "man")
dir.create(man, showWarnings = FALSE)

writeLines(c("\\\\name{foo}", "\\\\alias{foo}", "\\\\alias{bar}",
             "\\\\title{A Minimal Help Page}",
             "\\\\description{Nothing to see here.}"),
           file.path(man, "foo.Rd"))

rdTitle("foo", man = man)
#> Warning: /tmp/RtmpeEpvYf/man/foo.Rd:1: unexpected '{'
#> [1] NA
rdTitle("bar", man = man)          # aliases are matched as well
#> Warning: /tmp/RtmpeEpvYf/man/foo.Rd:1: unexpected '{'
#> [1] NA
rdTitle("nothing", man = man)      # NA
#> Warning: /tmp/RtmpeEpvYf/man/foo.Rd:1: unexpected '{'
#> [1] NA

unlink(man, recursive = TRUE)
```
