#' List of Correlations to Matrix 
#' 
#' Convert a named list of correlations to a correlation matrix with named 
#' dimensions.
#' 
#' @param x A named list of correlations (see example below).
#' @return Retruns a correlation matrix with named dimensions.
#' @export
#' @examples
#' x1 <- list(disc=c(.2, .4, .2), EE=c(.67, .54), PA=c(.86), DP=c())
#' MDlist2matrix(x1)
#' 
#' x2 <- list(disc=c(.6), BO=c())
#' MDlist2matrix(x2)
#' 
#' x3 <- list(disc=c(), EE=c(.2), PA=c(.4, .67), DP=c( .2, .54, .86))
#' MDlist2matrix(x3)
#' 
#' x4 <- list(disc=c(.2, .4, .2), EE=c(), PA=c(), DP=c())
#' MDlist2matrix(x4)
#' 
#' x5 <- list(disc=c(), EE=c(.2), PA=c(.4), DP=c(.2))
#' MDlist2matrix(x5)
#' 
#' \dontrun{
#' x6 <- list(disc=c(.2), EE=c(.2), PA=c(.4), DP=c(.2))
#' MDlist2matrix(x6) #error expected
#' }
#' 
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' dat <- read.MD(path)
#' MDapply(dat, "correlation_triangle", MDlist2matrix, "cor_mats")
MDlist2matrix <- function(x) {
    if(length(x) == 1) {
        if(is.null(x)){
            return(NA)
        }
        if(is.na(x)){
            return(NA)
        }
    }
    if(is.character(x)) {
        x <- eval(parse(text = x))	
    }
    lens <- sapply(x, length)
    if (all(diff(lens) == -1)) {
        dims <- length(x)
        mat <- matrix(rep(NA, dims^2), dims)
        dimnames(mat) <- list(names(x), names(x))
        diag(mat) <- 1
        mat[lower.tri(mat)] <- unlist(x)
        mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
        class(mat) <- "full_matrix"
    } else {
        if (all(diff(lens) == 1)) {
            x <- rev(x)
            dims <- length(x)
            mat <- matrix(rep(NA, dims^2), dims)
            dimnames(mat) <- list(rev(names(x)), rev(names(x)))
            diag(mat) <- 1
            mat[upper.tri(mat)] <- rev(unlist(lapply(x, rev)))
            mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
            class(mat) <- "full_matrix"
        } else {
            if (diff(lens)[1] < 0 && all(diff(lens)[-1] == 0)) {
                dims <- length(x)
                mat <- matrix(rep(NA, dims^2), dims)
                dimnames(mat) <- list(names(x), names(x))
                diag(mat) <- 1
                mat[1, ][is.na(mat[1, ])] <- unlist(x)
                mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
                class(mat) <- "incomplete_matrix"
            } else {
                if (diff(lens)[1] > 0 && all(diff(lens)[-1] == 0)) {
                    dims <- length(x)
                    mat <- matrix(rep(NA, dims^2), dims)
                    dimnames(mat) <- list(names(x), names(x))
                    diag(mat) <- 1
                    mat[1, ][is.na(mat[1, ])] <- unlist(x)
                    mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
                    class(mat) <- "incomplete_matrix"
                } else { 
                    stop("incorrect correlation structure provided")
                }
            }
        }
    }
    mat
}


#' Prints a full_matrix object
#' 
#' Prints a full_matrix object.
#' 
#' @param x The full_matrix object
#' @param \ldots ignored
#' @method print full_matrix
#' @S3method print full_matrix
print.full_matrix <- function(x, ...) {
    print(unclass(x), ...)
}


#' Prints an incomplete_matrix
#' 
#' Prints an incomplete_matrix.
#' 
#' @param x The incomplete_matrix object
#' @param \ldots ignored
#' @method print incomplete_matrix
#' @S3method print incomplete_matrix
print.incomplete_matrix <- function(x, ...) {
    print(unclass(x), ...)
}

