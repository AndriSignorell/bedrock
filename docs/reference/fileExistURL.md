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

  character string. The full URL to check.

- timeout:

  numeric. Timeout in seconds for the HTTP request. Default is 5.

## Value

logical value indicating whether the resource exists (`TRUE`) or not
(`FALSE`).

The returned value has additional attributes:

- `status`: HTTP status code returned by the server (e.g. 200, 404).

- `error`: error message (if a request error occurred).

## Details

The function first sends an HTTP `HEAD` request to minimize data
transfer. If the server responds with a status indicating that `HEAD`
itself is not supported (403, 405, 501), a `GET` request is attempted as
a fallback. A plain 404 is taken at face value, so that non-existing
files do not trigger a second request.

If the request fails (e.g., due to network issues or invalid URLs), the
function returns `FALSE` and stores the error message as an attribute.

## See also

Other file.path: [`buildPath()`](buildPath.md),
[`findDownload()`](findDownload.md), [`isFilePath()`](isFilePath.md),
[`isURL()`](isURL.md), [`splitPath()`](splitPath.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fileExistURL("https://www.example.com/data.csv")

res <- fileExistURL("https://invalid-url.test/file.csv")
attr(res, "status")
attr(res, "error")
} # }
```
