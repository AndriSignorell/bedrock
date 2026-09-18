# Extract Data Description from Excel File

Reads a documentation sheet from an Excel file and extracts variable
descriptions and coding information.

## Usage

``` r
dataDescription(fn, sheet = "Description")
```

## Arguments

- fn:

  character string. Path to the Excel file.

- sheet:

  character string. Name of the documentation sheet. Default is
  `"Description"`.

## Value

a list with the following components:

- `desctable`: A data frame containing the description table.

- `codes`: A named list of code definitions per variable.

## Details

The function reads the specified sheet and trims trailing empty rows.

If a column named `"Codes"` is present, its contents are split by line
breaks and returned as a list of codes per variable, keyed by the
`"Variable"` column.

The Excel sheet is expected to contain at least:

- Variable names

- Descriptions

- Optional coding definitions

If the sheet does not exist or no additional sheets are present, the
function returns `NULL`.

## See also

Other label.import: [`openDataObject()`](openDataObject.md)

## Examples

``` r
fn <- system.file("extdata", "example.xlsx", package = "bedrock")

desc <- dataDescription(fn)
desc$desctable
#>       Variable                 Beschreibung                   Codes   Skala
#> 1           id Identifier of the respondent                    <NA>  metric
#> 2       gender        Sex of the respondent        1=female\n2=male nominal
#> 3          age                 Age in years                    <NA>  metric
#> 4 satisfaction         Overall satisfaction 1=low\n2=medium\n3=high ordinal
desc$codes[["gender"]]
#> [1] "1=female" "2=male"  
```
