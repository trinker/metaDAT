#' Apply Function to Elements of a MDlist
#'
#' A convienence wrapper for \code{MDget} and optionally \code{MDassign}.  
#' For more complicated needs (i.e. working with more than one variable or pieces 
#' of a variable) use \code{MDget} to retrieve variable(s) and then lapply with 
#' select functions. Finally, reassign with \code{MDassign}.
#' 
#' @param meta.obj An MDlist (an object from \code{read.MD}).
#' @param var A length one character vector of the variable name to grab.
#' @param FUN A function that takes a single argument.
#' @param newvar A length one character vector of the variable name to assign to.
#' @return Returns a metaDAT list object with the newly assigned values.
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' dat <- MDapply(dat, "correlation_triangle", MDlist2matrix, "cor_mats")
#' MDapply(dat, "correlation_triangle", MDlist2matrix)
#' mean2 <- function(x) mean(unlist(x), na.rm = TRUE)
#' MDapply(dat, "cor_mats", mean2)
MDapply <- function(meta.obj, var, FUN, newvar = NULL) {
    FUN <- as.character(substitute(FUN))
    output <- lapply(MDget(meta.obj = meta.obj, vars = var), match.fun(FUN))
    if (is.null(newvar)) {
        names(output) <- names(meta.obj)
        output
    } else {
        MDassign(meta.obj = meta.obj, var = newvar, values = output)
    }
}

