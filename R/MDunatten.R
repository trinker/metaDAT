#' Unattenuation of Correlation Matrices
#' 
#' Correct for attentuation.  Operates on a list of correlation matrices.
#'
#' @param cor.mats A list of correlation matrices.  Length of this list will 
#' be equal to the number of rows in the object passed to the reals argument. 
#' @param reals A matrix/data.frame of reliabilities.  The column names must 
#' match the column/rownames of the correlation matrices exactly 
#' (order does not matter).
#' @return Returns a list of correlation matrices corrected for attentuation.
#' @details Uses Spearman's (1904) formula:
#' \deqn{\rho = \frac{r_{xy}}{\sqrt{r_{xx}r_{yy}}}}
#' If a realibility is missing (\code{NA}) 1 will be used as a conservative 
#' estimate.
#' @references Spearman, C. (1904) The proof and measurement of association 
#' between two things. The American Journal of Psychology, 15(1), 72-101.
#' @note Reliability column names must match exactly(including case) the 
#' row/column names of the correlation matrices.  If needed add a column of 
#' \code{NA}s.
#' @keywords attentuation
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' realiabilities <- do.call(cbind, lapply(MDget(dat, c("disc_phi", 
#'     "dp_phi", "ee_phi", "pa_phi")), as.numeric))
#' rownames(realiabilities) <- names(dat)
#' correlations <- MDapply(dat, "correlation_triangle", MDlist2matrix)
#' #matching reliability column names to correlation row/col names
#' colnames(realiabilities) <- c("disc", "EE", "PA", "DP")
#' rownames(realiabilities) == names(correlations) #sanity check
#' MDunatten(correlations, realiabilities)
#' #reassign
#' dat <- MDassign(dat, "att.cor", MDunatten(correlations, realiabilities))
#' dat 
MDunatten <- function(cor.mats, reals) {
    reals[is.na(reals)] <- 1
    corvects <- lapply(cor.mats, matrix2vectors)
    invisible(lapply(seq_along(corvects), function(i) {
        corvects[[i]][, "atten"] <<- rep(NA, nrow(corvects[[i]]))
    }))
    invisible(lapply(seq_along(corvects), function(i){
        den1 <- reals[i, match(corvects[[i]][, "row.var"], names(reals[i, ]))]
        den2 <- reals[i, match(corvects[[i]][, "col.var"], names(reals[i, ]))]
        corvects[[i]][, "atten"] <<- unlist(corvects[[i]][, "cors"]/sqrt(den1*den2))
    }))
    attens <- invisible(lapply(corvects, function(x) vectors2matrix(x[, -3])))
    attens
}