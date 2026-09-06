# Extract Variable Labels from Rd Documentation

Reads the variable descriptions out of the `\describe` section of a
documented dataset and returns them as a named character vector, the
names being the variable names. This turns documentation that already
exists into labels usable in tables, plots and codebooks, instead of
maintaining the same descriptions a second time in the code.

## Usage

``` r
rdLabels(dataName, package)
```

## Arguments

- dataName:

  character string, the name of the dataset.

- package:

  character string, the name of the package holding the dataset.

## Value

a named character vector of variable descriptions, the names being the
variable names.

## Details

The Rd database is read with
[`tools::Rd_db()`](https://rdrr.io/r/tools/Rdutils.html) and searched
recursively for the first `\describe` section, from which all
`\item{var}{description}` entries are taken. Only that first section is
read: on a page documenting more than one dataset, the labels of the
first one are returned.

Descriptions are returned as written in the Rd file, with whitespace and
line breaks collapsed to single spaces. Rd markup inside a description,
such as `\code{}` or `\eqn{}`, contributes its content without the
surrounding command.

The package must be installed, as the documentation is read from the
installed Rd database rather than from the sources.

## See also

[`tools::Rd_db()`](https://rdrr.io/r/tools/Rdutils.html)

Other pkg.funinfo: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funKeywords()`](funKeywords.md),
[`funList()`](funList.md), [`rdTitle()`](rdTitle.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rdLabels("Pizza", "bedrock")
## price               temperature         delivery_min
## "Price of the ..."  "Temperature ..."   "Delivery ..."
} # }
```
