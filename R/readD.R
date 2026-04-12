
#' Read a File from the Downloads Directory
#'
#' Reads a file from the Downloads directory and returns it as a data frame.
#' The file type is automatically detected from the extension.
#'
#' @param fname Character string. Name of the file.
#' @param base Logical; if \code{TRUE} (default), the result is converted
#' to a base R \code{data.frame} using \code{DescToolsX::toBaseR}.
#' @param ... Additional arguments passed to the underlying read function.
#'
#' @details
#' This is a convenience wrapper combining \code{\link{findDownload}} with
#' common file readers:
#' \itemize{
#'   \item Excel files (\code{.xls}, \code{.xlsx}) via \code{readxl}
#'   \item CSV files via \code{readr::read_csv}
#'   \item TSV files via \code{readr::read_tsv}
#'   \item Text files via \code{readr::read_table}
#' }
#'
#' By default, the result is converted to a base R \code{data.frame}.
#'
#' @return
#' A \code{data.frame} (or tibble if \code{base = FALSE}).
#'
#' @seealso
#' \code{\link{findDownload}},
#' \code{\link[readxl]{read_excel}},
#' \code{\link[readr]{read_csv}}
#'
#' @examples
#' \dontrun{
#' # Read Excel file
#' readD("data.xlsx")
#'
#' # Read CSV file
#' readD("data.csv")
#'
#' # Keep tibble output
#' readD("data.csv", base = FALSE)
#' }
#'


#' @export
readD <- function(fname, ..., base = TRUE) {
  
  file <- findDownload(fname)
  ext  <- tolower(tools::file_ext(file))
  
  res <- switch(ext,
                xls  = readxl::read_excel(file, ...),
                xlsx = readxl::read_excel(file, ...),
                csv  = readr::read_csv(file, ...),
                tsv  = readr::read_tsv(file, ...),
                txt  = readr::read_table(file, ...),
                stop(sprintf("Unsupported file type: %s", ext))
  )
  
  if (base) toBaseR(res) else res
}


