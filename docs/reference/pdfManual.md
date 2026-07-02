# Open CRAN PDF manual of a package

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

  Package name (symbol or character)

## See also

Other file.utils: [`buildPath()`](buildPath.md),
[`fileExistURL()`](fileExistURL.md),
[`findDownload()`](findDownload.md),
[`parseSASDatalines()`](parseSASDatalines.md),
[`readDownload()`](readDownload.md), [`splitPath()`](splitPath.md)
