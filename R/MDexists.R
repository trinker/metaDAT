#' Check Variable Existance
#'
#' Logical test to check the existance of variables.
#' 
#' @param meta.obj An MDlist (an object from \code{read.MD}).
#' @param vars A character vector of variable names to check existance.
#' @seealso 
#' \code{\link[base]{exists}}
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' MDexists(dat, "title")
MDexists <- function(meta.obj, vars) {
    name.list <- lapply(meta.obj, names)
    testexist <- function(var, name.list) {
        sapply(name.list, function(x) sum(vars %in% x) > 0)
    } 
    tests <- lapply(vars, function(x) {
        testexist(var = x, name.list = name.list)
    })
    if(length(vars) == 1) {
        tests <- unlist(tests)
    } else {
        names(tests) <- vars
    }
    tests  
}
