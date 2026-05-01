
#' List Keywords For R Manual Pages
#' 
#' List the keywords for specific R man pages or return a list of valid R
#' keywords.
#' 
#' If \code{topic} is provided, return a list of the Keywords associated with
#' \code{topic}.  Otherwise, display the list of valid R Keywords from the R
#' doc/Keywords file.
#' 
#' @param topic optional, object or man page topic
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[utils]{help}}
#' @keywords documentation
#' @examples
#' \donttest{
#' ## Show all valid R Keywords
#' funKeywords()
#' 
#' ## Show Keywords associated with the 'merge' function
#' funKeywords(merge)
#' funKeywords("merge")
#' }
#' 
#' 


#' @family pkg.introspection
#' @concept package-utilities
#' @concept data-inspection
#'
#'
#' @export
funKeywords <- function( topic ) {
  
  # verbatim from library(gtools)
  
  file <- file.path(R.home("doc"),"KEYWORDS")
  if(missing(topic))
  {
    file.show(file)
  } else {
    
    ## Local copy of trim.character to avoid cyclic dependency with gdata ##
    trim <-  function(s) {

      s <- sub(pattern="^[[:blank:]]+", replacement="", x=s)
      s <- sub(pattern="[[:blank:]]+$", replacement="", x=s)
      s
    }
    
    kw <- scan(file=file, what=character(), sep="\n", quiet=TRUE)
    kw <- grep("&", kw, value=TRUE)
    kw <- gsub("&[^&]*$","", kw)
    kw <- gsub("&+"," ", kw)
    kw <- na.omit(trim(kw))
    
    ischar <- tryCatch(is.character(topic) && length(topic) ==
                         1L, error = identity)
    if (inherits(ischar, "error"))
      ischar <- FALSE
    if (!ischar)
      topic <- deparse(substitute(topic))
    
    item <- paste("^",topic,"$", sep="")
    
    # old, replaced by suggestion of K. Hornik 23.2.2015
    # topics <- function(k) help.search(keyword=k)$matches[,"topic"]
    
    topics <- function(k) {
      matches <- help.search(keyword=k)$matches
      matches[ , match("topic", tolower(colnames(matches)))]
    }
    
    matches <- lapply(kw, topics)
    names(matches) <- kw
    
    tmp <- unlist(lapply( matches, function(m) grep(item, m, value=TRUE) ))
    names(tmp)
  }
}

