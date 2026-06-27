# Extract variable labels from Rd documentation

Extracts variable descriptions from the `\describe` section of a
dataset's Rd documentation and returns them as a named character vector.
The names correspond to variable names and the values to their
descriptions.

## Usage

``` r
getRdLabels(dataName, package)
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

[`bedrock::label`](label.md)

Other pkg.introspection: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funKeywords()`](funKeywords.md),
[`funList()`](funList.md), [`mergeArgs()`](mergeArgs.md)

## Examples

``` r
# Extract labels from a package dataset
if (FALSE) { # \dontrun{
getRdLabels("Pizza", "bedrock")
} # }
```
