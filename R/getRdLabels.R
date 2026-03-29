
#' Extract variable labels from Rd documentation
#'
#' Extracts variable descriptions from the \code{\\describe} section of a dataset's
#' Rd documentation and returns them as a named character vector. The names
#' correspond to variable names and the values to their descriptions.
#'
#' This function is useful for automatically generating variable labels from
#' documented datasets in R packages.
#'
#' @param data_name Character string. Name of the dataset.
#' @param package Character string. Name of the package containing the dataset.
#'
#' @return A named character vector where names are variable names and values
#'   are their corresponding descriptions extracted from the Rd file.
#'
#' @details
#' The function parses the Rd database via \code{\link[tools:Rd_db]{tools::Rd_db}}
#' and recursively searches for the \code{\\describe} section. It then extracts
#' all \code{\\item\{var\}\{description\}} entries.
#'
#' The function is fully CRAN-compliant and does not rely on internal (non-exported)
#' functions.
#'
#' @family topic.dataTools
#' @concept Metadata
#' @concept Documentation
#' @concept Programming Utilities 
#' 
#' @examples
#' # Extract labels from a package dataset
#' \dontrun{
#' getRdLabels("d.pizza", "bedrock")
#' }
#'
#' @seealso \code{\link[bedrock:Label]{bedrock::Label}}
#'


#' @export
getRdLabels <- function(data_name, package) {
  
  if (!requireNamespace("bedrock", quietly = TRUE)) {
    stop("Package 'bedrock' is required.")
  }
  
  if (missing(package)) {
    stop("Please provide a package name.")
  }
  
  # --- 1. Rd database laden ---
  rd_db <- tools::Rd_db(package)
  
  # Namen sehen typischerweise so aus: "d.pizza.Rd"
  rd_name <- paste0(data_name, ".Rd")
  
  if (!rd_name %in% names(rd_db)) {
    stop("No Rd entry found for ", data_name)
  }
  
  rd <- rd_db[[rd_name]]
  
  # --- 2. \describe finden ---
  desc_node <- .findRdTag(rd, "\\describe")
  

  if (is.null(desc_node)) {
    stop("No \\describe section found.")
  }
  
  # --- 3. Items extrahieren ---
  labels <- list()
  
  for (item in desc_node) {
    if (attr(item, "Rd_tag") == "\\item") {
      
      # remove white space and line breaks
      var <- gsub("\\s+", "", paste(unlist(item[[1]]), collapse = ""))
      desc <- trimws(paste(unlist(item[[2]]), collapse = " "))
      
      labels[[var]] <- desc
    }
  }
  
  return( unlist(labels) )

}



# == internal helper functions =====================================

.findRdTag <- function(x, tag) {
  
  # --- only check whether Rd_tag exists ---
  if (!is.null(attr(x, "Rd_tag"))) {
    if (attr(x, "Rd_tag") == tag) return(x)
  }
  
  # --- recursively through lists ---
  if (is.list(x)) {
    for (el in x) {
      res <- .findRdTag(el, tag)
      if (!is.null(res)) return(res)
    }
  }
  
  NULL
  
}



