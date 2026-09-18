# Preview a Delimited Text File

Read the first `n` data rows of a delimited text file and return the
result as a base R `data.frame` (a kind of
[`head()`](https://rdrr.io/r/utils/head.html) for files).

## Usage

``` r
peekFile(file, n = 10, ..., output = c("data.frame", "tibble"))
```

## Arguments

- file:

  character string specifying the file name.

- n:

  integer specifying the number of data rows to read, defaults to 10.

- ...:

  additional arguments passed to
  [`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html),
  e.g. `delim` or `skip`. The arguments `n_max` and `show_col_types` are
  managed internally and will be ignored if supplied; `guess_max`
  defaults to `n` but may be overridden.

- output:

  character, either `"data.frame"` (default) or `"tibble"`, determining
  the class of the returned object. Conversion to `data.frame` is done
  by [`toBaseR()`](toBaseR.md). The argument can be abbreviated. Note
  that it must be given as a named argument, as it follows the dots.

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

[`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html),
[`toBaseR()`](toBaseR.md),
[`head()`](https://rdrr.io/r/utils/head.html),

Other file.io: [`parseSASDatalines()`](parseSASDatalines.md),
[`pdfManual()`](pdfManual.md), [`readDownload()`](readDownload.md)

## Examples

``` r
# a small file to look into
fn <- tempfile(fileext = ".csv")
write.csv(iris, fn, row.names = FALSE)

if (requireNamespace("readr", quietly = TRUE)) {

  peekFile(fn, delim = ",")

  # unrepresentative early rows: guess types over more lines
  peekFile(fn, n = 5, delim = ",", guess_max = 150)
}
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa

unlink(fn)
```
