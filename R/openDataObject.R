
#' Load Excel Data with Metadata (Codes and Labels)
#'
#' Downloads an Excel file from a remote server and imports it as a data frame.
#' Optionally processes a documentation sheet to assign variable labels and
#' convert variables into factors with labeled levels.
#'
#' @param name Character string. File name including extension (e.g. \code{"data.xlsx"}).
#' @param url Character string. Base URL where the file is located.
#'   Defaults to \url{https://www.signorell.net/hwz/datasets/}.
#' @param doc List or \code{NA}. Defines the structure of the documentation sheet.
#'   If \code{NULL}, the function tries to detect a sheet named \code{"Description"}.
#'   If \code{NA}, no metadata processing is performed.
#' @param ... Additional arguments passed to \code{readxl::read_excel()}.
#'
#' @return
#' A \code{data.frame} containing the imported data. If metadata is available:
#' \itemize{
#'   \item Variables may be converted to factors (nominal/ordinal)
#'   \item Factor levels are labeled using provided codes
#'   \item Variable labels are assigned using \code{label()}
#' }
#'
#' @details
#' The function downloads the Excel file to a temporary location and reads the
#' first sheet as the main dataset.
#'
#' If a documentation sheet is available, it is expected to contain columns such as:
#' \itemize{
#'   \item Variable name
#'   \item Description (label)
#'   \item Codes (e.g. "1=Male\\r\\n2=Female")
#'   \item Scale ("nominal", "ordinal", etc.)
#' }
#'
#' Variables with scale \code{"nominal"} or \code{"ordinal"} are converted to
#' factors. Data values without a matching entry in the codes column become
#' \code{NA}.
#'
#' @examples
#' \dontrun{
#' # Load dataset with automatic metadata detection
#' openDataObject("example.xlsx")
#'
#' # Load dataset without metadata processing
#' openDataObject("example.xlsx", doc = NA)
#' }
#'
#'
#' @family label.utils
#' @concept label
#' @concept attribute
#' @export
openDataObject <- function(name, url = NULL, doc = NULL, ...) {

  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("readxl", quietly = TRUE))
    stop("Packages 'httr' and 'readxl' are required for this function.")

  if (is.null(url))
    url <- "https://www.signorell.net/hwz/datasets/"

  fullUrl <- paste0(sub("/+$", "", url), "/", name)
  
  tf <- tempfile(fileext = ".xlsx")
  
  resp <- httr::GET(fullUrl, httr::write_disk(tf, overwrite = TRUE))
  
  if (httr::http_status(resp)$category != "Success") {
    stop(sprintf("Download failed [%s]", httr::status_code(resp)))
  }
  
  z <- as.data.frame(readxl::read_excel(tf))
  
  sheets <- readxl::excel_sheets(tf)
  
  if (is.null(doc)) {
    if (length(sheets) > 1)
      doc <- list(Description = c("Variable", "Beschreibung", "Codes", "Skala"))
    else
      doc <- NA
  }
  
  if (!isNA(doc)) {
    
    doc_sheet <- names(doc)
    cols <- doc[[1]]
    
    code <- as.data.frame(readxl::read_excel(tf, sheet = doc_sheet))
    
    # trim trailing empty rows
    emptyRows <- which(apply(code, 1, function(x) all(is.na(x))))
    if (length(emptyRows) > 0) {
      code <- code[seq_len(min(emptyRows) - 1), , drop = FALSE]
    }
    
    col_var   <- cols[1]
    col_lbl   <- cols[2]
    col_code  <- cols[3]
    col_scale <- cols[4]
    
    # define factors
    id <- which(code[[col_scale]] %in% c("nominal", "ordinal"))
    
    if (length(id) > 0) {
      
      codes <- lapply(strsplit(code[[col_code]][id], "\r?\n"), function(x)
        strsplit(x, "=")
      )
      names(codes) <- code[[col_var]][id]
      
      for (v in code[[col_var]][id]) {

        z[[v]] <- factor(
          z[[v]],
          ordered = code[[col_scale]][code[[col_var]] == v] == "ordinal"
        )

        if (!is.null(codes[[v]]) && !all(is.na(unlist(codes[[v]])))) {

          # note: data values without a matching code become NA
          levels(z[[v]]) <- trimws(sapply(codes[[v]], `[`, 2))[
            match(
              levels(z[[v]]),
              trimws(sapply(codes[[v]], `[`, 1))
            )
          ]
        }
      }
    }
    
    # set variable labels
    for (v in code[[col_var]]) {
      lbl <- na.omit(code[[col_lbl]][code[[col_var]] == v])
      if (length(lbl) == 1L)
        label(z[[v]]) <- as.character(lbl)
    }
  }
  
  return(z)
}

