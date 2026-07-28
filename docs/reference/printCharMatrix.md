# Pretty-print a character matrix with alignment, spacing and column splitting

Prints a character matrix to the console with configurable alignment,
column spacing, optional row/column names, optional cli-based styling,
and automatic splitting into column blocks if the output exceeds the
console width.

## Usage

``` r
printCharMatrix(
  m,
  align = "right",
  sep = 2,
  showRownames = TRUE,
  showColnames = TRUE,
  useCliStyle = FALSE,
  width = getOption("width")
)
```

## Arguments

- m:

  a matrix (or object coercible to a matrix) containing values that will
  be converted to character for display.

- align:

  character vector specifying alignment of cell contents, either
  `"right"` (default) or `"left"`. A single value is recycled across all
  columns; alternatively a vector of length `ncol(m)` sets the alignment
  per column.

- sep:

  integer. Number of spaces between columns. Default is `2`.

- showRownames:

  logical. Should row names be printed? Default is `TRUE`.

- showColnames:

  logical. Should column names be printed? Default is `TRUE`.

- useCliStyle:

  logical. If `TRUE`, column names and row names are styled using
  [`cli::style_bold()`](https://cli.r-lib.org/reference/ansi-styles.html).
  Default is `FALSE`.

- width:

  integer. Maximum output width (in characters). Defaults to
  `getOption("width")`. If the table exceeds this width, it is split
  into column blocks that are printed one after another.

## Value

invisibly returns `NULL`. The formatted table is printed to the console.

## Details

The function formats all entries as character strings and computes
column widths dynamically. `NA` entries are shown as `"NA"`. If the full
table does not fit into the specified `width`, it is split column-wise
into multiple blocks (cell contents themselves are never wrapped). In
this case, row names and column headers are repeated for each block.

If a single column is wider than `width`, that column is printed on its
own and the requested `width` is deliberately exceeded, since a column
cannot be split further.

Alignment is applied per column, and spacing between columns is
controlled via `sep`. The function is designed as a lightweight
alternative to
[`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html) with
more control over formatting, making it suitable for reporting outputs
in packages.

## See also

Other data.print: [`columnWrap()`](columnWrap.md)

## Examples

``` r
m <- matrix(c(
  "50.575","50.543","45.207",
  "49.900","51.400","44.300",
  "5.106","8.192","10.197"
), nrow = 3, byrow = TRUE)

rownames(m) <- c("mean","median","sd")
colnames(m) <- c("Brent","Camden","Westminster")

# Default (right-aligned)
printCharMatrix(m)
#>          Brent  Camden  Westminster
#> mean    50.575  50.543       45.207
#> median  49.900  51.400       44.300
#> sd       5.106   8.192       10.197

# Left-aligned with custom spacing
printCharMatrix(m, align = "left", sep = 4)
#>           Brent     Camden    Westminster
#> mean      50.575    50.543    45.207     
#> median    49.900    51.400    44.300     
#> sd        5.106     8.192     10.197     

# With CLI styling (requires cli package)
if (requireNamespace("cli", quietly = TRUE)) {
  printCharMatrix(m, useCliStyle = TRUE)
}
#>          Brent  Camden  Westminster
#> mean    50.575  50.543       45.207
#> median  49.900  51.400       44.300
#> sd       5.106   8.192       10.197

# Force splitting into column blocks by reducing width
printCharMatrix(m, width = 20)
#>          Brent
#> mean    50.575
#> median  49.900
#> sd       5.106
#> 
#>         Camden
#> mean    50.543
#> median  51.400
#> sd       8.192
#> 
#>         Westminster
#> mean         45.207
#> median       44.300
#> sd           10.197
```
