
#' Samples for Combinations of a Set
#'
#' Return the value sets of combinations.
#'
#' @details
#' Depending on \code{output}, the result is returned either as:
#'
#' \itemize{
#'   \item a matrix with one combination per row
#'   \item a flat list where each element represents one combination
#' }
#'
#' If \code{m} contains more than one value, combinations are generated
#' independently for each value of \code{m}.
#'
#' @param x A vector of numeric values or characters.
#'   Character values need not be unique.
#' @param m Number of elements to choose.
#'   May be a vector.
#' @param replace Logical. Should repetition of the same element
#'   be allowed? Default is \code{FALSE}.
#' @param ordered Logical. Does the order matter?
#'   Default is \code{FALSE}.
#' @param output Character string specifying the output representation.
#'   One of:
#'
#'   \describe{
#'     \item{\code{\"matrix\"}}{
#'       Return combinations as a matrix.
#'     }
#'     \item{\code{\"list\"}}{
#'       Return combinations as a flat list.
#'     }
#'   }
#'
#'   Default is \code{\"matrix\"}.
#'
#' @return
#' If \code{output = \"matrix\"}:
#'
#' \itemize{
#'   \item a matrix with one combination per row
#'   \item if \code{length(m) > 1}, a list of matrices
#' }
#'
#' If \code{output = \"list\"}:
#'
#' \itemize{
#'   \item a flat list with one element per combination
#' }
#'
#' @seealso
#' \code{\link{combPairs}},
#' \code{\link{combn}},
#' \code{\link{choose}},
#' \code{\link{factorial}},
#' \code{vignette("Combinatorics")}
#'
#' @examples
#' x <- letters[1:4]
#' m <- 2
#'
#' # combinations with replacement
#' combSet(x, m, replace = TRUE, ordered = FALSE)
#'
#' # ordered combinations with replacement
#' combSet(x, m, replace = TRUE, ordered = TRUE)
#'
#' # ordered combinations without replacement
#' combSet(x, m, replace = FALSE, ordered = TRUE)
#'
#' # unordered combinations without replacement
#' combSet(x, m, replace = FALSE, ordered = FALSE)
#'
#' # return as flat list
#' x <- letters[1:5]
#'
#' combSet(
#'   x = x,
#'   m = c(1, 3, 5),
#'   output = "list"
#' )
#'
#' @rdname combinatoric

#' @family combinatorics  
#' @concept combinatorics  
#' @concept sampling
#'
#'
#' @export
combSet <- function(x,
                    m,
                    replace = FALSE,
                    ordered = FALSE,
                    output = c("matrix", "list")) {
  
  output <- match.arg(output)
  
  if (length(m) > 1L) {
    
    res <- lapply(
      m,
      function(i)
        combSet(
          x       = x,
          m       = i,
          replace = replace,
          ordered = ordered,
          output  = "matrix"
        )
    )
    
  } else {
    
    if (replace) {
      
      res <- as.matrix(
        do.call(
          expand.grid,
          as.list(as.data.frame(replicate(m, x)))
        )
      )
      
      dimnames(res) <- NULL
      
      if (!ordered) {
        res <- unique(t(apply(res, 1L, sort)))
      }
      
    } else {
      
      if (ordered) {
        
        # permn() returns a matrix directly
        res <- do.call(
          rbind,
          combn(x, m = m, FUN = permn, simplify = FALSE)
        )
        
      } else {
        
        res <- t(combn(x, m))
      }
    }
  }
  
  if (output == "list") {
    
    if (is.list(res)) {
      
      res <- do.call(
        c,
        lapply(
          res,
          function(x) {
            as.list(
              as.data.frame(t(x), stringsAsFactors = FALSE)
            )
          }
        )
      )
      
    } else {
      
      res <- as.list(
        as.data.frame(
          t(res),
          stringsAsFactors = FALSE
        )
      )
    }
    
    names(res) <- NULL
  }
  
  res
  
}

