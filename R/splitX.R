#' Split Data into Groups (Extended Interface)
#'
#' Splits a vector or object into groups defined by a factor or grouping variables.
#' This is a wrapper around \code{\link[base]{split}} with an additional
#' formula interface.
#'
#' @param x Object to be split (typically a vector).
#' @param f A factor or list of factors defining the groups (default method).
#' @param formula A formula of the form \code{y ~ group} or \code{y ~ g1 + g2}
#' specifying the variable to split (\code{y}) and the grouping variables.
#' @param data A data frame containing the variables in the formula.
#' @param subset Optional logical expression indicating rows to include.
#' @param na.action A function specifying how to handle missing values
#' (e.g., \code{\link[stats]{na.omit}}).
#' @param drop Logical; if \code{TRUE}, unused factor levels are dropped.
#' @param ... Further arguments passed to \code{\link[base]{split}}.
#'
#' @details
#' \code{splitX} extends \code{\link[base]{split}} by providing:
#' \itemize{
#'   \item An S3 interface
#'   \item A formula method for convenient specification of variables
#'   \item Support for multiple grouping variables via formula
#' }
#'
#' The formula interface evaluates a \code{\link[stats]{model.frame}} and splits
#' the response variable by one or more grouping variables.
#'
#' If multiple grouping variables are provided, the data are split by their
#' interaction (similar to \code{split(..., interaction(...))}).
#'
#' @return
#' A list of subsets of \code{x}, grouped according to \code{f} or the
#' grouping variables in the formula.
#'
#' @examples
#' # Default usage
#' x <- 1:10
#' g <- rep(letters[1:2], each = 5)
#' splitX(x, g)
#'
#' # Formula interface
#' df <- data.frame(
#'   y = rnorm(10),
#'   g1 = rep(letters[1:2], each = 5),
#'   g2 = rep(1:2, times = 5)
#' )
#'
#' splitX(y ~ g1, data = df)
#' splitX(y ~ g1 + g2, data = df)
#'
#' @family data.manipulation
#' @concept data-manipulation
#' @concept vector-manipulation
#' @concept factor-handling
#'
#'
#' @export
splitX <- function(x, ...) {
  UseMethod("splitX")
}

#' @rdname splitX
#' @export
splitX.default <- function(x, f, drop=FALSE, ...){
  split(x=x, f=f, drop=drop, ...) 
}


# experimental: formula interface for split

# raw early approach:
# split.formula <- function(x, f, drop = FALSE, data = NULL, ...) {
#   mf <- model.frame(x, data)
#   f <- mf[,2]
#   x <- mf[,1]
#   split(x, f, drop=drop, ...)
# }

#' @rdname splitX
#' @export
splitX.formula <- function(formula, data, subset, na.action, drop=FALSE, ...){
  
  if (missing(formula) || (length(formula) != 3L))
    stop("'formula' missing or incorrect")
  
  m <- match.call(expand.dots = FALSE)
  m$formula <- formula
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  
  m[[1L]] <- quote(stats::model.frame)
  # in order to delete potentially provided tolerance or
  # na.rm arguments to be passed later on
  # m$... <- NULL
  
  ## >>> IMPORTANT: Treat subset correctly due to collision with
  ##                the subset function.
  if (!missing(subset)) {
    m$subset <- substitute(subset)  # capture the argument
  } else {
    m$subset <- NULL                # remove completely
  }
  ## (Optional) na.action pass through unchanged:
  # if (missing(na.action)) m$na.action <- NULL
  
  mf <- eval(m, parent.frame())
  
  DNAME <- gettextf("%s by %s (rows)", names(mf)[1],
                    paste(names(mf)[-1], collapse=" : "))
  
  ff <- mf[, -1]
  if(ncol(mf[, -1, drop=FALSE]) > 1)
    ff <- as.list(mf[, -1])
  
  # now do the split
  res <- split(x=mf[, 1], f=ff, drop=drop, ...)
  
  # add na information
  if(!missing(na.action)){
    subj <- res[, names(mf)[2]]
    res <- na.action(res)
    # provide the names of omitted subjects
    attr(attr(res, "na.action"), "values") <- subj[as.numeric(attr(res, "na.action"))]
  }
  
  attr(res, "data.name") <- DNAME
  return(res)
  
}

