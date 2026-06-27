# Read a File from the Downloads Directory

Reads a file from the Downloads directory and returns it as a data
frame. The file type is automatically detected from the extension.

## Usage

``` r
readDownload(fname, ..., base = TRUE)
```

## Arguments

- fname:

  Character string. Name of the file.

- ...:

  Additional arguments passed to the underlying read function.

- base:

  Logical; if `TRUE` (default), the result is converted to a base R
  `data.frame` using `DescToolsX::toBaseR`.

## Value

A `data.frame` (or tibble if `base = FALSE`).

## Details

This is a convenience wrapper combining
[`findDownload`](findDownload.md) with common file readers:

- Excel files (`.xls`, `.xlsx`) via `readxl`

- CSV files via
  [`readr::read_csv`](https://readr.tidyverse.org/reference/read_delim.html)

- TSV files via
  [`readr::read_tsv`](https://readr.tidyverse.org/reference/read_delim.html)

- Text files via
  [`readr::read_table`](https://readr.tidyverse.org/reference/read_table.html)

By default, the result is converted to a base R `data.frame`.

## See also

[`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html),
[`read_csv`](https://readr.tidyverse.org/reference/read_delim.html)

Other file.utils: [`buildPath()`](buildPath.md),
[`fileExistURL()`](fileExistURL.md),
[`findDownload()`](findDownload.md), [`pdfManual()`](pdfManual.md),
[`splitPath()`](splitPath.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read Excel file
readDownload("data.xlsx")

# Read CSV file
readDownload("data.csv")

# Keep tibble output
readDownload("data.csv", base = FALSE)
} # }
```
