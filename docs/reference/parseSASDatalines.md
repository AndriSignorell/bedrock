# Parse SAS DATALINES/CARDS blocks into a data.frame

A parser for simple SAS dataline command texts. A `data.frame` is being
built with the columnnames listed in the input section.

## Usage

``` r
parseSASDatalines(x, validateNames = FALSE)
```

## Arguments

- x:

  a single character string containing a SAS DATA step with a DATALINES,
  CARDS, or CARDS4 block.

- validateNames:

  logical. If `TRUE` (default `FALSE`), emits a warning when the dataset
  name violates SAS naming rules.

## Value

a data.frame with column names taken from the INPUT statement. The
attribute `sas_dataset_name` carries the DATA step name. For
`DATA _NULL_` the data is still parsed and returned; the caller decides
what to do with it (matching SAS semantics). SAS missing-value markers
(`.`) are converted to `NA`.

## Details

The SAS function `DATA` is designed for quickly creating a dataset from
scratch. The whole step normally consists out of the `DATA` part
defining the name of the dataset, an `INPUT` line declaring the
variables and a `DATALINES` command followed by the values.  
The default delimiter used to separate the different variables is a
space (thus each variable should be one word). The \$ after the variable
name indicates that the variable preceding contain character values and
not numeric values. Without specific instructions, SAS assumes that
variables are numeric. The function will fail, if it encounters a
character in the place of an expected numeric value.  
Each new row in datalines will create a corresponding unique row in the
dataset. Notice that a ; is not needed after every row, rather it is
included at the end of the entire data step.

More complex command structures, i.e. other delimiters (dlm), in the
`INPUT`-section are not (yet) supported.

Only free-format (list) input is supported. The following SAS features
are intentionally rejected with an informative error:

- Column pointers (`@`, `@`)

- Column input (e.g. `var 1-10`)

- Formatted input (`:`)

Character values must not contain spaces or quotes; scan-based parsing
splits on whitespace and does not handle quoted strings.

## See also

Other file.io: [`pdfManual()`](pdfManual.md),
[`peekFile()`](peekFile.md), [`readDownload()`](readDownload.md)

## Examples

``` r
sas_code <- "
  data mydata;
    input name $ age score;
  datalines;
  Alice 30 95.5
  Bob   25 88.0
  ;
"
df <- parseSASDatalines(sas_code)
```
