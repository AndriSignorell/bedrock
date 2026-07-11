# Convert Custom Date Format to strftime Format

Translates a custom date format string using tokens like `yyyy`, `mm`,
`dd`, `mmm`, etc. into a valid `strftime`-compatible format string
(C-style).

## Usage

``` r
asCDateFmt(fmt)
```

## Arguments

- fmt:

  character string. Custom date format.

## Value

Character string. A valid `strftime` format.

## Details

The function parses the input string sequentially and replaces
recognized tokens while leaving all other characters unchanged. This
makes it robust to compact formats (e.g. `yyyymmdd`) and mixed text.

Supported tokens:

- `d`, `dd`, `ddd`, `dddd`

- `m`, `mm`, `mmm`, `mmmm`

- `y`, `yy`, `yyyy`

Mapping:

- `yyyy` -\> `%Y`

- `yy`, `y` -\> `%y`

- `mm`, `m` -\> `%m`

- `mmm` -\> `%b`

- `mmmm` -\> `%B`

- `dd` -\> `%d`

- `d` -\> `%e`

- `ddd` -\> `%a`

- `dddd` -\> `%A`

## Examples

``` r
asCDateFmt("yyyy-mm-dd")
#> [1] "%Y-%m-%d"
asCDateFmt("dd.mm.yy")
#> [1] "%d.%m.%y"
asCDateFmt("yyyymmdd")
#> [1] "%Y%m%d"
asCDateFmt("mmm d, yyyy")
#> [1] "%b %e, %Y"
```
