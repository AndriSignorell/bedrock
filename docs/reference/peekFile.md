# Preview a Delimited Text File

Read the first `n` data rows of a delimited text file and return the
result as a base R `data.frame`.

## Usage

``` r
peekFile(file, n = 10, ..., output = c("data.frame", "tibble"))
```

## Arguments

- file:

  character string specifying the file name

- n:

  integer specifying the number of data rows to read, defaults to 10

- ...:

  additional arguments passed to
  [`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html),
  e.g. `delim` or `skip`. The arguments `n_max` and `show_col_types` are
  managed internally and will be ignored if supplied; `guess_max`
  defaults to `n` but may be overridden.

- output:

  character, either `"data.frame"` (default) or `"tibble"`, determining
  the class of the returned object. Conversion to `data.frame` is done
  by [`toBaseR`](toBaseR.md). The argument can be abbreviated. Note that
  it must be given as a named argument, as it follows the dots.

## Value

a `data.frame` or a tibble (according to `output`) containing the first
`n` data rows of the file.

## Details

This function is intended for quickly inspecting large text files,
including compressed files supported by
[`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html).

Column types are guessed from the previewed rows only (the default
`guess_max` equals `n`). If early rows are not representative, supply a
larger `guess_max` via the dots.

## See also

[`read_delim`](https://readr.tidyverse.org/reference/read_delim.html),
[`toBaseR`](toBaseR.md)

Other file.io: [`parseSASDatalines()`](parseSASDatalines.md),
[`pdfManual()`](pdfManual.md), [`readDownload()`](readDownload.md)

## Examples

``` r
if (FALSE) { # \dontrun{
peekFile("data.csv")
peekFile("data.csv.gz", delim = "|", n = 20)

# unrepresentative early rows: guess types over more lines
peekFile("data.csv", n = 10, guess_max = 1000)
} # }
```
