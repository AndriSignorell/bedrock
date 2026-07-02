# Check if a File Exists at a URL

Performs an HTTP request to determine whether a resource exists at a
given URL. Uses a `HEAD` request by default and falls back to `GET` if
necessary.

## Usage

``` r
fileExistURL(url, timeout = 5)
```

## Arguments

- url:

  Character string. The full URL to check.

- timeout:

  Numeric. Timeout in seconds for the HTTP request. Default is 5.

## Value

Logical value indicating whether the resource exists (`TRUE`) or not
(`FALSE`).

The returned value has additional attributes:

- `status`: HTTP status code returned by the server (e.g. 200, 404)

- `error`: Error message (if a request error occurred)

## Details

The function first sends an HTTP `HEAD` request to minimize data
transfer. If the server responds with an error status (\>= 400), a `GET`
request is attempted as a fallback because some servers do not properly
support `HEAD`.

If the request fails (e.g., due to network issues or invalid URLs), the
function returns `FALSE` and stores the error message as an attribute.

## See also

Other file.utils: [`buildPath()`](buildPath.md),
[`findDownload()`](findDownload.md),
[`parseSASDatalines()`](parseSASDatalines.md),
[`pdfManual()`](pdfManual.md), [`readDownload()`](readDownload.md),
[`splitPath()`](splitPath.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fileExistURL("http://www.example.com/data.csv")

res <- fileExistURL("http://invalid-url.test/file.csv")
attr(res, "status")
attr(res, "error")
} # }
```
