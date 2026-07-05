# Read a File from the Downloads Directory

Reads a file from the Downloads directory and returns it as a data
frame. The file type is automatically detected from the extension.

## Usage

``` r
readDownload(file, ..., output = c("data.frame", "tibble"))
```

## Arguments

- file:

  character string specifying the name of the file.

- ...:

  additional arguments passed to the underlying read function, e.g.
  `sheet` for Excel files or `delim` for text files.

- output:

  character, either `"data.frame"` (default) or `"tibble"`, determining
  the class of the returned object. Conversion to `data.frame` is done
  by [`toBaseR`](toBaseR.md). The argument can be abbreviated. Note that
  it must be given as a named argument, as it follows the dots.

## Value

a `data.frame` or a tibble, according to `output`.

## Details

This is a convenience wrapper combining
[`findDownload`](findDownload.md) with common file readers:

- Excel files (`.xls`, `.xlsx`) via
  [`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)

- CSV files via
  [`readr::read_csv`](https://readr.tidyverse.org/reference/read_delim.html)

- TSV files via
  [`readr::read_tsv`](https://readr.tidyverse.org/reference/read_delim.html)

- Text files (`.txt`) via
  [`readr::read_delim`](https://readr.tidyverse.org/reference/read_delim.html),
  which guesses the delimiter from the file content

For the readr-based formats the column specification message is
suppressed by default; supply `show_col_types = TRUE` to restore it. By
default, the result is converted to a base R `data.frame`.

## See also

[`findDownload`](findDownload.md), [`peekFile`](peekFile.md),
[`toBaseR`](toBaseR.md),
[`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html),
[`read_csv`](https://readr.tidyverse.org/reference/read_delim.html)

Other io: [`peekFile()`](peekFile.md)

## Author

Andri Signorell <andri@signorell.net>

## Examples

``` r
if (FALSE) { # \dontrun{
# Read Excel file
readDownload("data.xlsx")

# Read CSV file
readDownload("data.csv")

# Keep tibble output
readDownload("data.csv", output = "tibble")
} # }
```
