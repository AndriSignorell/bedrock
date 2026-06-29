# List Keywords For R Manual Pages

List the keywords for specific R man pages or return a list of valid R
keywords.

## Usage

``` r
funKeywords(topic)
```

## Arguments

- topic:

  optional, object or man page topic

## Details

If `topic` is provided, return a list of the Keywords associated with
`topic`. Otherwise, display the list of valid R Keywords from the R
doc/Keywords file.

## See also

[`help`](https://rdrr.io/r/utils/help.html)

Other pkg.introspection: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funList()`](funList.md),
[`rdLabels()`](rdLabels.md), [`mergeArgs()`](mergeArgs.md)

## Author

Gregory R. Warnes <greg@warnes.net>

## Examples

``` r
# \donttest{
## Show all valid R Keywords
funKeywords()

## Show Keywords associated with the 'merge' function
funKeywords(merge)
#> [1] "data"  "manip" "array"
funKeywords("merge")
#> [1] "data"  "manip" "array"
# }

```
