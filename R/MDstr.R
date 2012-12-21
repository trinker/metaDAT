#' Display the Class of MDlist Variables
#' 
#' Provides the class of each MDlist variable.  Similar to \code{str} though 
#' less detailed.
#' 
#' @param meta.obj An MDlist (an object from \code{read.MD}).
#' @seealso
#' \code{\link[utils]{str}}
#' @keywords structure
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' MDstr(dat)
#' dat2 <- read.MD(path, , stringsAsFactors = TRUE)
#' MDstr(dat2)
MDstr <- function(meta.obj) {
    nms <- MDnames(meta.obj)
    data.frame(do.call(rbind, lapply(seq_along(nms), function(i) {
        c(var = nms[i], class = class(MDget(meta.obj, nms[i])))
    })), row.names =NULL)
}

