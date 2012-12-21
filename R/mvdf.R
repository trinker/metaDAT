#' Reorder Matrix for Analysis  
#' 
#' \code{mvdf} is not intended for general use. The function takes a list of 
#' correlations matrices, grabs the lower triangles and diagonals and unlists by 
#' columns.
#' 
#' @param cor.mats A square, symetrical matrix with ones on the diagonal (a 
#' correlation matrix).
#' @param sep A character string to separate the variable names.
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' dat <- MDapply(dat, "correlation_triangle", MDlist2matrix, "cor_mats")
#' mats <- MDget(dat, "cor_mats", s.names=TRUE)
#' mvdf(mats)
#' mvdf(mats, "_x_")
mvdf <- function(cor.mats, sep = "_") {
    vcovmat <- do.call(rbind, lapply(seq_along(cor.mats), function(i) {
        w <- cor.mats[[i]][lower.tri(cor.mats[[i]], TRUE)]
        names(w) <- paste0(rep(colnames(cor.mats[[i]]),  
            nrow(cor.mats[[i]]):1), sep, colnames(cor.mats[[i]]))
        w
    }))
    rownames(vcovmat) <- names(cor.mats)
    vcovmat 
}