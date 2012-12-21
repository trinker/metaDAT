#' Extract Meta Variables
#'
#' Extract variables from each study for analysis.
#' 
#' @param meta.obj An MDlist (an object from \code{read.MD}).
#' @param vars A character vector of variable names to extract.
#' @param s.names logical.  If TRUE attempts to name the extracted vector/list 
#' elements with study names.
#' @param by.study logical.  If TRUE orders the variables by study rather  than 
#' by variable.
#' @seealso
#' \code{\link[base]{get}}
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' MDget(dat, c("nt"))
#' MDget(dat, c("title", "nt"), TRUE)
#' MDget(dat, c("title", "nt"))
#' MDget(dat, "grade_level")
#' MDget(dat, "cor_mats")
#' MDget(dat, "cor_mats", s.names = TRUE)
#' dat <- MDapply(dat, "correlation_triangle", MDlist2matrix, "cor_mats")
#' MDget(dat, c("title", "nt", "cor_mats"))
#' MDget(dat, c("grade_level", "nt", "cor_mats"), by.study=TRUE)
#' MDget(dat, c("grade_level", "nt", "cor_mats"), s.names = TRUE, by.study=TRUE)
MDget <- function(meta.obj, vars, s.names = FALSE, by.study = FALSE) {
    mtget <- function(meta.obj, vars, s.names) {
        varlist <- lapply(meta.obj,  "[", vars)
        nulls <- sapply(unlist(varlist, recursive = FALSE), is.null)
        varlist[nulls] <- NA
        if (length(unlist(varlist)) > length(meta.obj)) { 
            varvect <- unlist(varlist, recursive = FALSE)
            type.test <<- TRUE
        } else {
            varvect <- unlist(varlist)      
            type.test <<- FALSE 
        }
        if (s.names) {
            names(varvect) <- names(meta.obj)
        } else {
            names(varvect) <- NULL
        }    
        varvect
    }
    vlist <- lapply(vars, function(x) {
        mtget(meta.obj = meta.obj, vars = x, s.names = s.names)
    })
    if(length(vars) == 1) {
        if (type.test) {
            vlist <- unlist(vlist, recursive = FALSE)
        } else {
            vlist <- unlist(vlist)
        }
    } else {
        names(vlist) <- vars
    }
    if (by.study) {
        vlist <- lapply(seq_along(meta.obj), function(i){
            x <- lapply(seq_along(vlist), function(j) {
                vlist[[c(j, i)]] 
            })
            names(x) <- vars
            x
        })
        if (s.names) {
            names(vlist) <- names(meta.obj)
        }
    }
    vlist   
}

