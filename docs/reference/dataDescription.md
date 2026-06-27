# Extract Data Description from Excel File

Reads a documentation sheet from an Excel file and extracts variable
descriptions and coding information.

## Usage

``` r
dataDescription(fn, sheet = "Description")
```

## Arguments

- fn:

  Character string. Path to the Excel file.

- sheet:

  Character string. Name of the documentation sheet. Default is
  `"Description"`.

## Value

A list with the following components:

- `desctable`: A data frame containing the description table

- `codes`: A named list of code definitions per variable

## Details

The function reads the specified sheet and trims trailing empty rows.

If a column named `"Codes"` is present, its contents are split by line
breaks (`\r\n`) and returned as a list of codes per variable.

The Excel sheet is expected to contain at least:

- Variable names

- Descriptions

- Optional coding definitions

If the sheet does not exist or no additional sheets are present, the
function returns `NULL`.

## See also

Other label.utils: [`label()`](label.md),
[`openDataObject()`](openDataObject.md)

## Examples

``` r
if (FALSE) { # \dontrun{
desc <- dataDescription("example.xlsx")

desc$desctable
desc$codes[["gender"]]
} # }
```
