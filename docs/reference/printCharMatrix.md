# Pretty-print a character matrix with alignment, spacing and wrapping

Prints a character matrix to the console with configurable alignment,
column spacing, optional row/column names, optional cli-based styling,
and automatic wrapping if the output exceeds the console width.

## Usage

``` r
printCharMatrix(
  m,
  align = c("right", "left"),
  sep = 2,
  showRownames = TRUE,
  showColnames = TRUE,
  useCliStyle = FALSE,
  width = getOption("width")
)
```

## Arguments

- m:

  A matrix (or object coercible to a matrix) containing values that will
  be converted to character for display.

- align:

  Character string specifying alignment of cell contents. Either
  `"right"` (default) or `"left"`.

- sep:

  Integer. Number of spaces between columns. Default is `2`.

- showRownames:

  Logical. Should row names be printed? Default is `TRUE`.

- showColnames:

  Logical. Should column names be printed? Default is `TRUE`.

- useCliStyle:

  Logical. If `TRUE`, column names and row names are styled using
  [`cli::style_bold()`](https://cli.r-lib.org/reference/ansi-styles.html).
  Default is `FALSE`.

- width:

  Integer. Maximum output width (in characters). Defaults to
  `getOption("width")`. If the table exceeds this width, it is split
  into column blocks and printed in multiple sections.

## Value

Invisibly returns `NULL`. The formatted table is printed to the console.

## Details

The function formats all entries as character strings and computes
column widths dynamically. If the full table does not fit into the
specified `width`, it is split column-wise into multiple blocks. In this
case, row names and column headers are repeated for each block.

Alignment is applied per column, and spacing between columns is
controlled via `sep`. The function is designed as a lightweight
alternative to
[`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html) with
more control over formatting, making it suitable for reporting outputs
in packages.

## See also

Other table.utils: [`collapseTable()`](collapseTable.md),
[`multMerge()`](multMerge.md)

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
#>   mean  50.575  50.543       45.207 
#> median  49.900  51.400       44.300 
#>     sd   5.106   8.192       10.197 
#> 

# Left-aligned with custom spacing
printCharMatrix(m, align = "left", sep = 4)
#>           Brent     Camden    Westminster 
#> mean      50.575    50.543    45.207      
#> median    49.900    51.400    44.300      
#> sd        5.106     8.192     10.197      
#> 

# With CLI styling (requires cli package)
if (requireNamespace("cli", quietly = TRUE)) {
  printCharMatrix(m, useCliStyle = TRUE)
}
#>          Brent  Camden  Westminster 
#>   mean  50.575  50.543       45.207 
#> median  49.900  51.400       44.300 
#>     sd   5.106   8.192       10.197 
#> 

# Force wrapping by reducing width
printCharMatrix(m, width = 20)
#>          Brent 
#>   mean  50.575 
#> median  49.900 
#>     sd   5.106 
#> 
#>         Camden 
#>   mean  50.543 
#> median  51.400 
#>     sd   8.192 
#> 
#>         Westminster 
#>   mean       45.207 
#> median       44.300 
#>     sd       10.197 
#> 
```
