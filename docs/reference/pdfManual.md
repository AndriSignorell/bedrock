# Open CRAN PDF Manual of a Package

PDF versions of the manual are usually not included as vignettes in R
packages. Still this format is convenient for reading and doing full
text search.  
This function creates the appropriate link to the pdf file on CRAN and
opens the pdf manual in a browser window.

## Usage

``` r
pdfManual(package)
```

## Arguments

- package:

  package name (symbol or character).

## Value

the URL of the PDF manual, invisibly. Called for its side effect of
opening the browser.

## Details

A warning (not an error) is issued if the package is not installed
locally, as the manual may well exist on CRAN anyway.

## See also

[`browseURL`](https://rdrr.io/r/utils/browseURL.html)

Other file.io: [`parseSASDatalines()`](parseSASDatalines.md),
[`peekFile()`](peekFile.md), [`readDownload()`](readDownload.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pdfManual(DescToolsX)
pdfManual("bedrock")
} # }
```
