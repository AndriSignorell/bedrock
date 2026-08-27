# List Keywords For R Manual Pages

List the keywords for specific R man pages or return a list of valid R
keywords.

## Usage

``` r
funKeywords(topic)
```

## Arguments

- topic:

  optional, object or man page topic.

## Value

if `topic` is missing, the R keywords documentation file is opened for
display via [`file.show`](https://rdrr.io/r/base/file.show.html),
invisibly returning `NULL`. Otherwise, a character vector of topic names
whose keywords match `topic`.

## Details

If `topic` is provided, return a list of the Keywords associated with
`topic`. Otherwise, display the list of valid R Keywords from the R
doc/Keywords file.

## Note

Substantially based on the `keywords()` function from the gtools package
by Gregory R. Warnes, with minor adaptations by the package author.

## See also

[`help`](https://rdrr.io/r/utils/help.html)

Other pkg.funinfo: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funList()`](funList.md),
[`rdLabels()`](rdLabels.md), [`rdTitle()`](rdTitle.md)

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
