#' Fisher's z Transformation of Correlation Matrices
#' 
#' Stabalizes the variances of correlations.  Operates on a list of correlation 
#' matrices.
#'
#' @param cor.mats A list of correlation matrices.  
#' @return Returns a list of correlation matrices with Fisher's z transformation 
#' applied.
#' @details Uses Fisher's (1915) transformation:
#' \deqn{{z}' = .5ln \left(\frac{1 + r}{1 - r}\right)}
#' @references Fisher, R. A. (1915) Frequency distribution of the values of the 
#' correlation coefficient in samples from an indefinitely large population. 
#' Biometrika, Vol. 10, pp. 507-521.
#' @keywords attentuation
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' correlations <- MDapply(dat, "correlation_triangle", MDlist2matrix)
#' MDfishersz(correlations)
#' #reassign
#' dat <- MDassign(dat, "fishz.cor", MDfishersz(correlations))
#' dat 
#' 
#' #In less steps:
#' fishFUN <- function(x) MDfishersz(MDlist2matrix(x))
#' MDapply(dat, "correlation_triangle", fishFUN, newvar = "fishz.cor")
MDfishersz <- function(cor.mats) {
    fisherz <- function(r) .5*(log((1+r)/(1-r)))
    if(!is.list(cor.mats)) {
            corvect <- matrix2vectors(cor.mats)
            corvect$fz <- fisherz(corvect[, "cors"])
            vectors2matrix(corvect[, -3])
    } else {
        corvect <- lapply(cor.mats, matrix2vectors)
        lapply(corvect, function(x) {
            x$fz <- fisherz(x[, "cors"])
            vectors2matrix(x[, -3])
        })
    }
}