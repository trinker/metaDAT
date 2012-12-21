#' Assign Value to metaDAT List
#'
#' Assign value(s) to a new or existing MDlist variable
#' 
#' @param meta.obj An MDlist (an object from \code{read.MD}).
#' @param var A length one character vector of the variable name to assign to.
#' @param values A vector of value(s) to assign to var. 
#' @param NA2NULL logical.  If TRUE converts missing (NA) to NULL.
#' @return Returns a MDlist object with the newly assigned values.
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' MDassign(dat, "new", rnorm(3))
#' MDassign(dat, "nt", rnorm(3))
#' MDassign(dat, "new", list(1, 2, 3))
#' MDassign(dat, "new", list(1, 2, NULL))
#' MDassign(dat, "new", c(1, 2, NA))
#' MDassign(dat, "new", c(1, 2, NA), NA2NULL = FALSE)
#' MDassign(dat, "new", list(NA, mtcars[, 1], mtcars))
MDassign <- function(meta.obj, var, values, NA2NULL = TRUE){
    if (length(values) == 1) {
        values <- rep(values, length(meta.obj))
    }
    if (NA2NULL) {
        values <- lapply(values, function(x){
            if (is.null(x)||is.na(x)) {NULL} else {x}   
        })
    }
    if(is.list(values)) {
        LIST <- invisible(lapply(seq_along(meta.obj), function(i) {
            if (length(values[[i]]) == 1 && is.vector(values[[i]]) | is.null(values[[i]])) {
                meta.obj[[i]][var] <- values[[i]]
            } else {
                meta.obj[[i]][var] <- list(values[[i]])
            }
            return(meta.obj[[i]])
        }))
    } else {
        LIST <- invisible(lapply(seq_along(meta.obj), function(i) {
            meta.obj[[i]][var] <- values[i] 
            return(meta.obj[[i]])
        }))        
    }
    names(LIST) <- names(meta.obj)
    LIST
}



