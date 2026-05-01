
#' Load Excel Data with Metadata (Codes and Labels)
#'
#' Downloads an Excel file from a remote server and imports it as a data frame.
#' Optionally processes a documentation sheet to assign variable labels and
#' convert variables into factors with labeled levels.
#'
#' @param name Character string. File name including extension (e.g. \code{"data.xlsx"}).
#' @param url Character string. Base URL where the file is located.
#'   Defaults to \url{http://www.signorell.net/hwz/datasets/}.
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
#' Variables with scale \code{"nominal"} or \code{"ordinal"} are converted to factors.
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
#' @importFrom httr GET write_disk http_status status_code
#' @importFrom readxl read_excel excel_sheets
#' @seealso \code{\link{dataDescription}}, \code{\link{label}}
#'



#' @family label.utils
#' @concept variable-labels
#' @concept data-manipulation
#' @concept file-utilities
#'
#'
#' @export
openDataObject <- function(name, url = NULL, doc = NULL, ...) {
  
  if (is.null(url))
    url <- "http://www.signorell.net/hwz/datasets/"
  
  full_url <- paste0(url, name)
  
  tf <- tempfile(fileext = ".xlsx")
  
  resp <- httr::GET(full_url, httr::write_disk(tf, overwrite = TRUE))
  
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
  
  if (!is.na(doc)) {
    
    doc_sheet <- names(doc)
    cols <- doc[[1]]
    
    code <- as.data.frame(readxl::read_excel(tf, sheet = doc_sheet))
    
    # sichere NA-Zeilen-Erkennung
    empty_rows <- which(apply(code, 1, function(x) all(is.na(x))))
    if (length(empty_rows) > 0) {
      code <- code[1:(min(empty_rows) - 1), ]
    }
    
    col_var   <- cols[1]
    col_lbl   <- cols[2]
    col_code  <- cols[3]
    col_scale <- cols[4]
    
    # Faktoren definieren
    id <- which(code[[col_scale]] %in% c("nominal", "ordinal"))
    
    if (length(id) > 0) {
      
      codes <- lapply(strsplit(code[[col_code]][id], "\\r\\n"), function(x)
        strsplit(x, "=")
      )
      names(codes) <- code[[col_var]][id]
      
      for (x in code[[col_var]][id]) {
        
        z[[x]] <- factor(
          z[[x]],
          ordered = code[[col_scale]][code[[col_var]] == x] == "ordinal"
        )
        
        if (!is.null(codes[[x]]) && !all(is.na(unlist(codes[[x]])))) {
          
          levels(z[[x]]) <- trimws(sapply(codes[[x]], `[`, 2))[
            match(
              levels(z[[x]]),
              trimws(sapply(codes[[x]], `[`, 1))
            )
          ]
        }
      }
    }
    
    # Labels setzen
    for (x in code[[col_var]]) {
      label(z[[x]]) <- na.omit(code[[col_lbl]][code[[col_var]] == x])
    }
  }
  
  return(z)
}

