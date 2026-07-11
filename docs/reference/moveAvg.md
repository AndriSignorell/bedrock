# Moving Average

Computes a simple moving average (running mean) of a numeric vector or
time series.

## Usage

``` r
moveAvg(
  x,
  order,
  align = c("center", "left", "right"),
  endrule = c("NA", "keep", "constant", "trim")
)
```

## Arguments

- x:

  a univariate numeric vector or `ts` object. Matrices and multi-column
  objects are not supported.

- order:

  a single positive integer giving the window width. Must satisfy
  `1 <= order <= length(x)`.

- align:

  a character string controlling how the window is positioned relative
  to each output value:

  `"center"`

  :   Default. The window is centred on the current observation. For odd
      `order` the window is symmetric; for even `order` see Details.

  `"left"`

  :   The window starts at the current observation and extends to the
      right.

  `"right"`

  :   The window ends at the current observation and extends to the
      left.

- endrule:

  a character string indicating how boundary values (where a full window
  is unavailable) are handled:

  `"NA"`

  :   Default. Boundary values are left as `NA`.

  `"keep"`

  :   Boundary values are taken from the original `x`.

  `"constant"`

  :   Boundary values are filled with the nearest computed
      moving-average value.

  `"trim"`

  :   Boundary values are computed from all available observations in a
      progressively smaller window.

## Value

A vector of the same length and class as `x`, with `NA` at boundary
positions unless `endrule` specifies otherwise.

## Details

The core computation uses cumulative sums for O(n) efficiency: \$\$ \bar
x_i = \frac{1}{k}\sum\_{j} x\_{i+j} \$\$ where the summation range
depends on `align`.

**Even-order windows and center alignment**

For even `order`, centering is ambiguous. This implementation averages
the two adjacent right-aligned windows of width `order`, which is the
convention used by
[`forecast::ma()`](https://pkg.robjhyndman.com/forecast/reference/ma.html).

**Boundary handling (`endrule = "trim"`)**

At the boundaries the window is contracted to include only the available
observations. For center alignment with even `order`, the boundary
window width at position \\i\\ is \\i + \lfloor order/2 \rfloor\\.

**Missing values**

`NA` in `x` propagates through
[`cumsum()`](https://rdrr.io/r/base/cumsum.html) and will produce `NA`
in all moving-average values whose window contains that observation.
There is no `na.rm` option; pre-filter with `x[!is.na(x)]` if needed
(note this changes index positions).

## See also

[`zoo::rollmean()`](https://rdrr.io/pkg/zoo/man/rollmean.html),
[`forecast::ma()`](https://pkg.robjhyndman.com/forecast/reference/ma.html),
[`stats::runmed()`](https://rdrr.io/r/stats/runmed.html)

Other vector.window: [`midx()`](midx.md), [`quot()`](quot.md)

## Examples

``` r
moveAvg(AirPassengers, order = 5)
#>        Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec
#> 1949    NA    NA 122.4 127.0 133.0 136.2 137.6 137.2 131.0 125.0 118.4 116.4
#> 1950 120.8 127.0 128.4 135.2 144.0 149.8 154.4 156.0 149.0 143.0 138.0 136.4
#> 1951 145.4 155.2 161.6 168.2 178.0 182.2 186.4 184.4 178.0 171.4 165.8 165.0
#> 1952 171.2 178.2 181.6 191.0 201.0 210.8 216.4 218.0 208.8 201.6 192.4 189.8
#> 1953 198.8 211.4 218.4 227.8 241.4 248.6 249.0 245.4 232.8 220.2 206.6 196.8
#> 1954 201.6 211.0 217.6 229.6 252.4 264.0 270.4 269.4 257.2 242.6 232.4 227.2
#> 1955 234.8 248.0 256.2 270.8 297.0 313.0 321.6 322.4 306.8 289.6 277.0 270.0
#> 1956 278.6 293.8 301.8 319.8 347.0 364.6 373.0 370.6 350.0 328.6 310.6 299.8
#> 1957 309.8 325.2 335.0 356.4 389.2 411.4 422.6 421.0 397.6 371.8 346.4 329.2
#> 1958 332.2 340.8 346.2 365.2 399.8 428.4 439.6 438.8 413.8 383.0 354.0 341.6
#> 1959 351.0 368.2 384.8 407.2 448.4 479.0 492.4 489.8 467.8 439.2 410.8 396.4
#> 1960 398.8 418.6 432.0 455.6 501.8 539.2 548.6 546.4 517.4 479.4    NA    NA
moveAvg(AirPassengers, order = 5, endrule = "trim")
#>           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
#> 1949 120.6667 122.7500 122.4000 127.0000 133.0000 136.2000 137.6000 137.2000
#> 1950 120.8000 127.0000 128.4000 135.2000 144.0000 149.8000 154.4000 156.0000
#> 1951 145.4000 155.2000 161.6000 168.2000 178.0000 182.2000 186.4000 184.4000
#> 1952 171.2000 178.2000 181.6000 191.0000 201.0000 210.8000 216.4000 218.0000
#> 1953 198.8000 211.4000 218.4000 227.8000 241.4000 248.6000 249.0000 245.4000
#> 1954 201.6000 211.0000 217.6000 229.6000 252.4000 264.0000 270.4000 269.4000
#> 1955 234.8000 248.0000 256.2000 270.8000 297.0000 313.0000 321.6000 322.4000
#> 1956 278.6000 293.8000 301.8000 319.8000 347.0000 364.6000 373.0000 370.6000
#> 1957 309.8000 325.2000 335.0000 356.4000 389.2000 411.4000 422.6000 421.0000
#> 1958 332.2000 340.8000 346.2000 365.2000 399.8000 428.4000 439.6000 438.8000
#> 1959 351.0000 368.2000 384.8000 407.2000 448.4000 479.0000 492.4000 489.8000
#> 1960 398.8000 418.6000 432.0000 455.6000 501.8000 539.2000 548.6000 546.4000
#>           Sep      Oct      Nov      Dec
#> 1949 131.0000 125.0000 118.4000 116.4000
#> 1950 149.0000 143.0000 138.0000 136.4000
#> 1951 178.0000 171.4000 165.8000 165.0000
#> 1952 208.8000 201.6000 192.4000 189.8000
#> 1953 232.8000 220.2000 206.6000 196.8000
#> 1954 257.2000 242.6000 232.4000 227.2000
#> 1955 306.8000 289.6000 277.0000 270.0000
#> 1956 350.0000 328.6000 310.6000 299.8000
#> 1957 397.6000 371.8000 346.4000 329.2000
#> 1958 413.8000 383.0000 354.0000 341.6000
#> 1959 467.8000 439.2000 410.8000 396.4000
#> 1960 517.4000 479.4000 447.7500 427.6667
moveAvg(AirPassengers, order = 4, align = "right", endrule = "constant")
#>         Jan    Feb    Mar    Apr    May    Jun    Jul    Aug    Sep    Oct
#> 1949 122.75 122.75 122.75 122.75 125.00 129.25 133.25 138.00 141.75 137.75
#> 1950 114.00 115.75 125.00 129.25 131.75 137.50 144.75 153.50 161.75 157.75
#> 1951 133.00 137.25 153.25 159.00 165.75 172.75 178.00 187.00 190.00 186.00
#> 1952 161.25 165.75 177.50 181.25 184.25 193.75 203.00 218.25 224.75 218.00
#> 1953 188.25 189.50 205.50 215.75 224.00 235.75 242.75 252.00 254.00 246.00
#> 1954 199.00 193.25 207.00 213.50 221.00 240.00 256.75 273.25 279.50 270.75
#> 1955 225.75 226.75 242.75 252.75 259.75 280.25 304.50 324.00 334.50 324.25
#> 1956 268.25 269.00 289.00 297.75 306.25 330.50 354.50 377.50 386.75 369.75
#> 1957 299.50 298.25 319.50 330.00 340.00 370.25 397.50 427.25 439.50 420.75
#> 1958 332.00 324.75 339.00 342.00 347.75 377.00 409.25 448.50 458.75 439.75
#> 1959 341.50 337.25 361.25 376.00 391.00 423.50 459.00 499.75 510.50 494.25
#> 1960 397.75 393.75 408.00 422.00 435.75 471.75 522.50 558.75 567.75 549.25
#>         Nov    Dec
#> 1949 126.75 119.25
#> 1950 143.75 136.25
#> 1951 172.75 164.50
#> 1952 203.50 191.50
#> 1953 225.00 207.25
#> 1954 246.00 230.00
#> 1955 292.50 275.25
#> 1956 334.25 309.50
#> 1957 380.75 348.00
#> 1958 394.50 352.50
#> 1959 447.75 409.25
#> 1960 491.25 447.75
```
