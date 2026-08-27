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

  character string. Name of the dataset.

- package:

  character string. Name of the package containing the dataset.

## Value

a named character vector where names are variable names and values are
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

Other pkg.funinfo: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funKeywords()`](funKeywords.md),
[`funList()`](funList.md), [`rdTitle()`](rdTitle.md)

## Examples

``` r
# Extract labels from a package dataset
if (FALSE) { # \dontrun{
rdLabels("Pizza", "bedrock")
} # }
```
