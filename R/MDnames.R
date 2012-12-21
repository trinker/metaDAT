#' The Unique Names of a MDList
#' 
#' Function to get the unique names of an metaDAT list object.
#' 
#' @param meta.obj An MDlist (an object from \code{read.MD}).
#' @return Returns the unique names of a metaDAT list.
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' MDnames(dat)
MDnames <- function(meta.obj) sort(unique(unlist(lapply(meta.obj, names))))

