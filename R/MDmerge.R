#' Merge Two or More MDlists
#'
#' Merge two or more MDlists.
#' 
#' @param \ldots MDlists (objects from \code{read.MD}) to be coerced to one.
#' @param id.vars Specifications of the common variable(s).  Default is NULL.
#' @param sep A character string to separate the MDlist merged study names by + 
#' identifiers (id.vars).
#' @param detect.class logical.  If TRUE detects the class of the character 
#' string(s) for each variable.
#' @param stringsAsFactors logical. Should character vectors be converted to 
#' factors if detect.class is TRUE?
#' @seealso
#' \code{\link[base]{merge}}
#' @return Returns a merged MDlist object.
#' @note General use is for merging multiple coders with different studies 
#' together into one MDlist.  This is not recommended for combing the same 
#' studies coded by two different coders (double coded).  Instead keep two 
#' different MDlist data sets for reliability checking.
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat1 <- read.MD(path)
#' path2 <- system.file("extdata/bibTest3.bib", package = "metaDAT")
#' dat2 <- read.MD(path2)
#' datMerged <- MDmerge(dat2, dat1)
#' datMerged
#' MDstr(datMerged)
#' names(datMerged)
#' MDmerge(dat2, dat1, id.vars = c("coder", "time"), sep = "%")
MDmerge <- function(..., id.vars = NULL, sep = "_", detect.class = TRUE,
    stringsAsFactors = FALSE){
    nms <- as.character(substitute(list(...)))[-1L]
    metas <- lapply(nms, get)
    metasU <- unlist(metas, recursive=FALSE)
    if (length(id.vars) > 1) {
        id.vars <- paste2(MDget(metasU, id.vars), sep = sep)
    } else {
        id.vars <- MDget(metasU, id.vars)
    }
    if (!is.null(id.vars)) {
        if (any(is.na(id.vars))) {
            NAS <- names(metasU)[is.na(id.vars)]
            warning(paste0("\nThe following study(s) did not contain the id.var(s):\n\n",
                paste(NAS, collapse = ", ")))
        }
        names(metasU) <- paste0(names(metasU), sep , id.vars)
        metaU <- metasU[sort(names(metasU))]
    }
    if (detect.class) {
    	metaU <- MDclass(metaU, stringsAsFactors = stringsAsFactors)	
    }   
    metaU
}
