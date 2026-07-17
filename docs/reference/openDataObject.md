# Load Excel Data with Metadata (Codes and Labels)

Downloads an Excel file from a remote server and imports it as a data
frame. Optionally processes a documentation sheet to assign variable
labels and convert variables into factors with labeled levels.

## Usage

``` r
openDataObject(name, url = NULL, doc = NULL, ...)
```

## Arguments

- name:

  character string. File name including extension (e.g. `"data.xlsx"`).

- url:

  character string. Base URL where the file is located. Defaults to
  <https://www.signorell.net/hwz/datasets/>.

- doc:

  list or `NA`. Defines the structure of the documentation sheet. If
  `NULL`, the function tries to detect a sheet named `"Description"`. If
  `NA`, no metadata processing is performed.

- ...:

  additional arguments passed to
  [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html).

## Value

a `data.frame` containing the imported data. If metadata is available:

- variables may be converted to factors (nominal/ordinal).

- factor levels are labeled using provided codes.

- variable labels are assigned using [`label()`](label.md).

## Details

The function downloads the Excel file to a temporary location and reads
the first sheet as the main dataset.

If a documentation sheet is available, it is expected to contain columns
such as:

- Variable name

- Description (label)

- Codes (e.g. "1=Male\r\n2=Female")

- Scale ("nominal", "ordinal", etc.)

Variables with scale `"nominal"` or `"ordinal"` are converted to
factors. Data values without a matching entry in the codes column become
`NA`.

## See also

Other label.import: [`dataDescription()`](dataDescription.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load dataset with automatic metadata detection
openDataObject("example.xlsx")

# Load dataset without metadata processing
openDataObject("example.xlsx", doc = NA)
} # }

```
