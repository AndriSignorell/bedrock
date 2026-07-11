# Split a File Path into Its Components

Splits a file path into its components such as directory, file name, and
extension. The function is OS-aware and works on both Windows and
Unix-like systems.

## Usage

``` r
splitPath(path, lastIsFile = NULL)
```

## Arguments

- path:

  a character vector of file paths

- lastIsFile:

  logical; if `TRUE`, the last component of `path` is treated as a file
  name. If `FALSE`, it is treated as part of the directory path. If
  `NULL` (default), the function determines this automatically based on
  whether the path ends with a path separator.

## Value

A list with the following components (each a vector of the same length
as `path`):

- normpath:

  Normalized path as returned by
  [`normalizePath`](https://rdrr.io/r/base/normalizePath.html).

- drive:

  Drive letter on Windows systems (e.g., `"C:"`), otherwise `NA`.

- dirname:

  Directory path without drive letter, including trailing separator.

- fullfilename:

  Full file name including extension (if applicable).

- fullpath:

  Full directory path including drive letter and trailing separator.

- filename:

  File name without extension.

- extension:

  File extension without leading dot.

## Details

The function uses [`basename`](https://rdrr.io/r/base/basename.html) and
[`dirname`](https://rdrr.io/r/base/basename.html) for
platform-independent path handling. File name and extension are
extracted using
[`file_path_sans_ext`](https://rdrr.io/r/tools/fileutils.html) and
[`file_ext`](https://rdrr.io/r/tools/fileutils.html).

If `lastIsFile = FALSE`, the path is treated as a directory and
file-related components (`fullfilename`, `filename`, `extension`) are
returned as `NA`.

## See also

[`basename`](https://rdrr.io/r/base/basename.html),
[`dirname`](https://rdrr.io/r/base/basename.html),
[`file_ext`](https://rdrr.io/r/tools/fileutils.html),
[`file_path_sans_ext`](https://rdrr.io/r/tools/fileutils.html)

Other file.path: [`buildPath()`](buildPath.md),
[`fileExistURL()`](fileExistURL.md),
[`findDownload()`](findDownload.md), [`isFilePath()`](isFilePath.md),
[`isURL()`](isURL.md)

## Examples

``` r
splitPath("C:/temp/file.txt")
#> $normpath
#> [1] "C:/temp/file.txt"
#> 
#> $drive
#> [1] "C:"
#> 
#> $dirname
#> [1] "/temp/"
#> 
#> $fullfilename
#> [1] "file.txt"
#> 
#> $fullpath
#> [1] "C:/temp/"
#> 
#> $filename
#> [1] "file"
#> 
#> $extension
#> [1] "txt"
#> 

splitPath("/home/user/data.csv")
#> $normpath
#> [1] "C:/home/user/data.csv"
#> 
#> $drive
#> [1] "C:"
#> 
#> $dirname
#> [1] "/home/user/"
#> 
#> $fullfilename
#> [1] "data.csv"
#> 
#> $fullpath
#> [1] "C:/home/user/"
#> 
#> $filename
#> [1] "data"
#> 
#> $extension
#> [1] "csv"
#> 

# treat as directory
splitPath("/home/user/folder/", lastIsFile = FALSE)
#> $normpath
#> [1] "C:/home/user/folder/"
#> 
#> $drive
#> [1] "C:"
#> 
#> $dirname
#> [1] "/home/user/folder//"
#> 
#> $fullfilename
#> [1] NA
#> 
#> $fullpath
#> [1] "C:/home/user/folder//"
#> 
#> $filename
#> [1] NA
#> 
#> $extension
#> [1] NA
#> 
```
