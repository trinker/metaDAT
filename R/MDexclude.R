#' Include/Exclude Variables from an MDlist 
#' 
#' Convience tools to quickly include or exlcude variables from all 
#' studies of an MDlist object.  Similar functionality to \code{subset}.
#' 
#' @rdname MDexclude
#' @param meta.obj An MDlist (an object from \code{read.MD}).
#' @param vars A character vector of the variable to include/exclude.
#' @seealso
#' \code{\link[base]{subset}}
#' @return Returns an MDlist with just the included variables (\code{MDinclude}) 
#' or the excluded variables (\code{MDexclude}) removed from each study.
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' vars.list <- c("coder", "time", "ee_phi")
#' MDinclude(dat, vars.list)
#' MDexclude(dat, vars.list)
MDexclude <- function(meta.obj, vars) {
	lapply(meta.obj, function(x) x[!names(x) %in% c(vars)])
}

#' @rdname MDexclude
#' @export
MDinclude <- function(meta.obj, vars) {
	lapply(meta.obj, function(x) x[names(x) %in% c(vars)])
}
