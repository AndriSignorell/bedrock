# Load Course Dataset from Server

Downloads and loads a dataset from predefined course servers or a
user-defined URL.

## Usage

``` r
courseData(name, url = NULL, header = TRUE, sep = ";", ...)
```

## Arguments

- name:

  character string. File name including extension (e.g. `"data.csv"`).

- url:

  optional character string. Base URL where the file is located. If
  `NULL`, default course repositories are searched.

- header:

  logical. Whether the file contains a header row. Passed to
  [`read.table()`](https://rdrr.io/r/utils/read.table.html).

- sep:

  character. Field separator used in the file. Default is `";"`.

- ...:

  additional arguments passed to the underlying import functions such as
  [`read.table()`](https://rdrr.io/r/utils/read.table.html) or
  [`openDataObject()`](openDataObject.md)

## Value

A data frame or object returned by the respective import function:

- For text files: a `data.frame`

- For Excel files: an object returned by
  [`openDataObject()`](openDataObject.md)

## Details

If no `url` is provided, the function searches for the file in the
following locations:

- <https://www.signorell.net/hwz/datasets/>

- <https://www.signorell.net/buch/>

The first location where the file exists is used.

File type handling:

- `.xls`, `.xlsx`: loaded via [`openDataObject()`](openDataObject.md)

- other files: loaded via
  [`read.table()`](https://rdrr.io/r/utils/read.table.html)

## See also

Other datasets: [`Cards`](Cards.md), [`Pizza`](Pizza.md),
[`Roulette`](Roulette.md), [`Tarot`](Tarot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load from default repositories
courseData("mydata.csv")

# Load from custom URL
courseData("mydata.csv", url = "https://example.com/data/")
} # }
```
