
#' Read a File from the Downloads Directory
#'
#' Reads a file from the Downloads directory and returns it as a data
#' frame. The file type is automatically detected from the extension.
#'
#' This is a convenience wrapper combining [findDownload()] with
#' common file readers:
#' \itemize{
#'   \item Excel files (`.xls`, `.xlsx`) via
#'         `readxl::read_excel`
#'   \item CSV files via `readr::read_csv`
#'   \item TSV files via `readr::read_tsv`
#'   \item Text files (`.txt`) via `readr::read_delim`,
#'         which guesses the delimiter from the file content
#' }
#'
#' For the readr-based formats the column specification message is
#' suppressed by default; supply `show_col_types = TRUE` to restore
#' it. By default, the result is converted to a base R `data.frame`.
#'
#' @param file character string specifying the name of the file.
#' @param \dots additional arguments passed to the underlying read
#'   function, e.g. `sheet` for Excel files or `delim`
#'   for text files.
#' @param output character, either `"data.frame"` (default) or
#'   `"tibble"`, determining the class of the returned object.
#'   Conversion to `data.frame` is done by [toBaseR()].
#'   The argument can be abbreviated. Note that it must be given as a
#'   named argument, as it follows the dots.
#'
#' @return a `data.frame` or a tibble, according to `output`.
#'
#' @seealso [findDownload()],
#'   [toBaseR()], [readxl::read_excel()],
#'   [readr::read_csv()]
#'
#' @examples
#' \dontrun{
#' # Read Excel file
#' readDownload("data.xlsx")
#'
#' # Read CSV file
#' readDownload("data.csv")
#'
#' # Keep tibble output
#' readDownload("data.csv", output = "tibble")
#' }
#'
#' @family file.io
#' @concept table
#' @export
readDownload <- function(file, ..., output = c("data.frame", "tibble")) {

  output <- match.arg(output)

  file <- findDownload(file)
  ext  <- tolower(tools::file_ext(file))

  if (ext == "")
    stop(gettextf("file '%s' has no extension, cannot detect file type",
                  basename(file)),
         call. = FALSE)

  if (!ext %in% c("xls", "xlsx", "csv", "tsv", "txt"))
    stop(gettextf(
      "unsupported file type: '%s' (supported: xls, xlsx, csv, tsv, txt)",
      ext), call. = FALSE)

  # required reader package depends on the file type
  pkg <- if (ext %in% c("xls", "xlsx")) "readxl" else "readr"
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(gettextf("package '%s' is required for readDownload(), please install it",
                  pkg),
         call. = FALSE)

  dots <- list(...)

  # suppress readr's column spec message unless explicitly requested
  if (ext %in% c("csv", "tsv", "txt") && is.null(dots$show_col_types))
    dots$show_col_types <- FALSE

  res <- switch(ext,
                xls  = ,
                xlsx = do.call(readxl::read_excel, c(list(file), dots)),
                csv  = do.call(readr::read_csv,    c(list(file), dots)),
                tsv  = do.call(readr::read_tsv,    c(list(file), dots)),
                txt  = do.call(readr::read_delim,  c(list(file), dots))
  )

  if (output == "data.frame") toBaseR(res) else res
}
