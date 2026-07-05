# Locate a File in the Downloads Directory

Returns the full path to a file located in the user's Downloads
directory.

## Usage

``` r
findDownload(file)
```

## Arguments

- file:

  Character string. Name of the file.

## Value

A character string giving the full path to the file.

## Details

The function resolves the path to the user's Downloads directory using
an internal helper and appends `file`. It does not perform any
downloading; it only locates files that already exist locally.

If the file does not exist, an error is thrown.

## See also

Other file.utils: [`buildPath()`](buildPath.md),
[`fileExistURL()`](fileExistURL.md),
[`parseSASDatalines()`](parseSASDatalines.md),
[`pdfManual()`](pdfManual.md), [`splitPath()`](splitPath.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Locate a file in Downloads
findDownload("data.csv")
} # }
```
