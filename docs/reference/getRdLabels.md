# Extract variable labels from Rd documentation

Extracts variable descriptions from the `\describe` section of a
dataset's Rd documentation and returns them as a named character vector.
The names correspond to variable names and the values to their
descriptions.

## Usage

``` r
rdLabels(dataName, package)
```

## Arguments

- dataName:

  Character string. Name of the dataset.

- package:

  Character string. Name of the package containing the dataset.

## Value

A named character vector where names are variable names and values are
their corresponding descriptions extracted from the Rd file.

## Details

This function is useful for automatically generating variable labels
from documented datasets in R packages.

The function parses the Rd database via
[`tools::Rd_db`](https://rdrr.io/r/tools/Rdutils.html) and recursively
searches for the `\describe` section. It then extracts all
`\item{var}{description}` entries.

The function is fully CRAN-compliant and does not rely on internal
(non-exported) functions.

## See also

[`bedrock::label`](https://andrisignorell.github.io/bedrock/reference/label.md)

Other pkg.introspection:
[`funArgs()`](https://andrisignorell.github.io/bedrock/reference/funArgs.md),
[`funCalls()`](https://andrisignorell.github.io/bedrock/reference/funCalls.md),
[`funKeywords()`](https://andrisignorell.github.io/bedrock/reference/funKeywords.md),
[`funList()`](https://andrisignorell.github.io/bedrock/reference/funList.md),
[`mergeArgs()`](https://andrisignorell.github.io/bedrock/reference/mergeArgs.md)

## Examples

``` r
# Extract labels from a package dataset
if (FALSE) { # \dontrun{
rdLabels("Pizza", "bedrock")
} # }
```
