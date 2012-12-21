#' Auto Assign Classes
#' 
#' Detects the intended class of the like elements of an MDlist for each 
#' variable across studies.  The function only detects numeric or 
#' character/factor.
#' 
#' @param meta.obj An MDlist (an object from \code{read.MD}).
#' @param stringsAsFactors logical. Should character vectors be converted to 
#' factors? Default is FALSE.
#' @return Returns an MDlist structure with appropriate classes.
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' MDclass(dat)
#' MDclass(dat, stringsAsFactors = TRUE)
MDclass <- function(meta.obj, stringsAsFactors = FALSE) {
    nms <- MDnames(meta.obj)
    output <- invisible(lapply(seq_along(nms), function(i) {
        vars <- MDget(meta.obj, nms[i])
        if (all(suppressWarnings(!is.na(as.numeric(na.omit(vars)))))) {
            vars <- as.numeric(vars)
        } else {
            if (stringsAsFactors) {
                vars <- as.factor(vars)
            } else {
                vars <- as.character(vars)
            }
        }
        meta.obj <<- MDassign(meta.obj, nms[i], unlist(vars, recursive = TRUE))
    }))
    meta.obj
}
