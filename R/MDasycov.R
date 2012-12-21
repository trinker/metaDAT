#' Asymptotic Variances/Covariance
#' 
#' Produce asymptotic variances/covariance with optional correction for 
#' attentuation and Fisher's z transformation.
#'
#' @param cor.mats A list of correlation matrices.  If unattenuated supply the 
#' list biased correlations to the \code{biased.cor.mats} argument.  If biased
#' optionally supply a dataframe of reliabilities to the \code{reals} argument. 
#' @param n.vect A vector of sample sizes corresponding to the studies in the 
#' list of \code{cor.mats}.
#' @param outcome.var The name of the outcome variable the other measures 
#' correlate with.
#' @param reals A matrix/data.frame of reliabilities.  The column names must 
#' match the column/rownames of the correlation matrices exactly. 
#' (order does not matter).
#' @param fishersz logical.  If TRUE uses Fisher's z transformation.
#' @param mvmeta.out logical.  If TRUE outputs a list with the correlations and 
#' variances/covariances in a format that can be used by the \code{mvmeta} package.
#' @param biased.cor.mats A list of biased correlation matrices supplied if 
#' unattentuated correlations are passed to the \code{cor.mats} argument.
#' @param sep A character string to separate the variable names.
#' @return Returns either a list of matrices of asymptotic covariances/variances 
#' of the correlation matrices or (if \code{mvmeta.out} is TRUE) a list of a 
#' matrix of correlations in the form selected (raw, unattenuated or Fishers z 
#' transformed) and matrix of variances/covariances with studies as rownames 
#' that can be passed to \code{mvmeta} from the \code{mvmeta} package. 
#' @details Uses Olkin and Siotani's (1976) formulas:
#' \deqn{Var(r_{ist})\approx \frac{(1-\rho_{ist}^{2})^2}{n_i}}
#' \deqn{Cov(r_{ist}, r_{iuv})= [.5\rho_{ist}\rho_{iuv}(\rho_{isu}^2 + 
#' \rho_{isv}^2 + \rho_{itu}^2 + \rho_{itv}^2) + \rho_{isu}\rho_{itv}+  
#' \rho_{isv}\rho_{itu}-}
#' \deqn{(\rho_{ist}\rho_{isu}\rho_{isv} + 
#' \rho_{its}\rho_{itu}\rho_{itv}) + \rho_{ius}\rho_{iut}\rho_{iuv} + 
#' \rho_{ivs}\rho_{ivt}\rho_{ivu}]/n_i}
#' 
#' Unattenuated Variances Formula (Borenstein, Hedges, Higgins & Rothstein, 2009):
#' 
#' \deqn{V_{r}^u = \frac{Var(r_{ist})}{\rho/\rho^u}}
#' 
#' Where: \cr
#' \eqn{\rho} is the raw correlation \cr
#' \eqn{\rho^u} is the unattenuated correlation \cr
#' 
#' Fisher's z approach also requires:
#' 
#' \deqn{Var(z_{ist})\approx \frac{1}{n_i - 3}}
#' \deqn{Cov(z_{ist}, z_{iuv})= \frac{\sigma_{ist,uv}}{(1-\rho_{ist}^2)(1-\rho_{iuv}^2)}}
#' If a realibility is missing (\code{NA}) 1 will be used as a conservative 
#' estimate.
#' @references Olkin, I., & Siotani, M. (1976) Asymptotic distribution of
#'   functions of a correlation matrix. In Essays in Probability and Statistics 
#'   Chapter 16 (S. Ikeda, ed.) 235-251. Shinko Tsusho, Tokyo. 
#' 
#' Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R. (2009). Introduction to meta-
#'   analysis (1st ed.). West Sussex, UK: Wiley.
#'   
#' Becker, B. J. (2000).  Multivariate meta-analysis.  In H. A. Tinsley & S. Brown, 
#'   (Eds.).  Handbook of applied multivariate statistics and mathematical modeling. 
#'   San Diego: Academic Press. 
#' 
#' @note Reliability column names must match exactly(including case) the 
#' row/column names of the correlation matrices.  If needed add a column of 
#' \code{NA}s.
#' @keywords asymptotic covariance
#' @seealso
#' \code{\link[metaDAT]{MDfishersz}},
#' \code{\link[metaDAT]{MDunatten}},
#' \code{\link[mvmeta]{mvmeta}}
#' @export
#' @examples
#' #generating data sets
#' set.seed(10)
#' MAT <- function(n = 5) {
#'     x <- matrix(round(rnorm(n^2, 0, .3), 2), n)
#'     dimnames(x) <- list(LETTERS[1:n], LETTERS[1:n])
#'     diag(x) <-1
#'     x[upper.tri(x)] <- t(x)[upper.tri(x)]
#'     x
#' }
#' cor.mats1 <- lapply(1:5, function(i) MAT(4))
#' names(cor.mats1) <- paste0("study_", 1:length(cor.mats1))
#' cor.mats2 <- lapply(1:3, function(i) MAT())
#' names(cor.mats2) <- paste0("study_", 1:length(cor.mats2))
#' n1 <- sample(40:150, length(cor.mats1), TRUE)
#' n2 <- sample(40:150, length(cor.mats2), TRUE)
#' reliabilities <- matrix(round(rnorm(20, .7, .1), 2), ncol = 4)
#' dimnames(reliabilities) <- list(names(cor.mats1), LETTERS[1:4])
#' 
#' #Using MDasycov
#' MDasycov(cor.mats1, n1, outcome.var = "A", reals = reliabilities)
#' MDasycov(cor.mats1, n1, outcome.var = "D")
#' MDasycov(cor.mats1, n1, outcome.var = "C")
#' MDasycov(cor.mats2, n2, outcome.var = "B")
#' MDasycov(cor.mats2, n2, outcome.var = "E", mvmeta.out = FALSE)
#' MDasycov(cor.mats2, n2, outcome.var = "E", fishersz = TRUE)
#' 
#' #With the mvmeta package
#' \dontrun{
#' mvmDAT <- MDasycov(cor.mats1, n1, outcome.var = "A", reals = reliabilities)
#' library(mvmeta)
#' mvmeta(mvmDAT[[1]], mvmDAT[[2]])
#' }
MDasycov <- function(cor.mats, n.vect, outcome.var, reals = NULL,  
    fishersz = FALSE, mvmeta.out = TRUE, biased.cor.mats = NULL, 
    sep = "_x_") {
    if (!is.null(biased.cor.mats)) {
        raw.cors <- biased.cor.mats
    } else {
        raw.cors <- cor.mats
    }
    if (fishersz & !is.null(reals)) {
        warning("Attempting to unattenuate and transform with Fisher's z")
    }
    if (missing(outcome.var)) stop("outcome.var not specified") 
    if (length(cor.mats) != length(n.vect)) {
        stop("Length of cor.mats not equal to length of n.vect")
    }
    if (!is.null(reals) & is.null(biased.cor.mats)) {
        cor.mats <- MDunatten(cor.mats =cor.mats, reals = reals)
    } 
    if (fishersz) {
        cor.mats <- MDfishersz(cor.mats)
    }
    reorg <- function(cor.mat, outcome.var) {
        ovar <- which(colnames(cor.mat) %in% outcome.var)
        new.var <- cor.mat[, -ovar][-ovar, ]
        new.var <- cbind(cor.mat[, ovar][-ovar], new.var)
        new.var <- rbind(c(cor.mat[, ovar][ovar], 
            cor.mat[, ovar][-ovar]), new.var)
        rownames(new.var)[1] <- colnames(new.var)[1]
        new.var
    }
    mat2vect <- function(cor.mat) {
        nmscor <- colnames(cor.mat)
        cols <- nmscor[1:(length(nmscor)-1)]
        rows <- nmscor[2:length(nmscor)]
        rowdim <- 1:length(rows)
        row.var <- rows[unlist(lapply(seq_along(rowdim), function(i) {
            rowdim[i:length(rowdim)]
        }))]
        col.var <- rep(cols, length(cols):1)
        cors <- cor.mat[lower.tri(cor.mat)]
        data.frame(var1 = col.var, var2 = row.var, 
            cors = cors, stringsAsFactors = FALSE)
    }
    triad <- function(cor.vect, cor.mat) {
        dims <- 1:(nrow(cor.mat) - 1)
        grab1 <- rep(dims[-length(dims)], rev(dims[-length(dims)]))
        grab2 <- rep(dims[-1], dims[-length(dims)])
        tris <- lapply(seq_along(grab1), function(i) {
            covars <- cor.vect[c(grab1[i], grab2[i]), 2]
            midtri <- cor.vect[cor.vect[, 1] %in% covars & 
                cor.vect[, 2] %in% covars, 3]
            c(cor.vect[grab1[i], 3], cor.vect[grab2[i], 3], midtri)
        }) 
        nms <- paste2(cor.vect[, 1:2], "_")
        names(tris) <- sapply(seq_along(grab1), function(i) {
            paste(nms[c(grab1[i], grab2[i])], collapse="_")
        })
        tris
    }
    olkin <- function(triad.mat, n, fishersz) {
        olkin2 <- function(tri, n) {
            (.5*tri[1]*tri[2]*(1 + (tri[2]^2) + (tri[1]^2) + 
                (tri[3]^2)) + tri[3] + tri[2]*tri[1] - 
                (tri[1]*tri[2] + (tri[1]^2)*tri[3] + 
                tri[1]*tri[2] + (tri[2]^2)*tri[3]))/n
        }   #  okin2(c(.561, .393, .286), 66)
        fishers <- function(tri, x) {
            x/((1 - (tri[1]^2))*(1 - (tri[2]^2)))
        }
        if (fishersz) {
            sapply(triad.mat, function(x) {
                fishers(x, olkin2(x, n = n))
            })
        } else {
            sapply(triad.mat, function(x) {
                olkin2(x, n = n)
            })
        }
    }
    power.fun <- function(Cor.mats, N, Fishersz, outcome.var) {
        olkin(triad(mat2vect(reorg(Cor.mats, outcome.var)), Cor.mats), n = N, 
            fishersz = Fishersz)
    } 
    covars <- invisible(lapply(seq_along(n.vect), function(i) {
        power.fun(cor.mats[[i]], n.vect[[i]], Fishersz = fishersz, 
            outcome.var = outcome.var)
    }))
    news <- lapply(cor.mats, reorg, outcome.var)
    mdim <- sapply(news, function(x) nrow(x) - 1)
    mnms <- sapply(news, function(x) {
        paste0(colnames(x)[1], "_", colnames(x)[-1])
    })
    emat <- invisible(lapply(seq_along(mdim), function(i) {
        mat <- matrix(rep(NA, mdim[i]*mdim[i]), mdim[i])
        dimnames(mat) <- list(mnms[, i], mnms[, i])
        mat
    }))    
    names(emat) <- names(cor.mats)
    covmats <- invisible(lapply(seq_along(emat), function(i) {
        emat[[i]][lower.tri(emat[[i]])] <- covars[[i]]
        emat[[i]][upper.tri(emat[[i]])] <- t(covars[[i]]) 
        emat[[i]]
    }))
    if (fishersz) {
        covmats <- invisible(lapply(seq_along(n.vect), function(i) {
            diag(covmats[[i]]) <- 1/(n.vect[[i]] - 3)
            covmats[[i]]
        }))
    } else {
        variances <- function(r, n) ((1 - (r^2))^2)/(n - 1)
        if (is.null(reals) & is.null(biased.cor.mats)){
            covmats <- invisible(lapply(seq_along(cor.mats), function(i) {
                diag(covmats[[i]]) <- variances(mat2vect(reorg(cor.mats[[i]], outcome.var))[, 
                    3], n.vect[[i]])[1:nrow(covmats[[i]])]
                covmats[[i]]
            }))
        } else {
            if (!is.null(reals) & !is.null(biased.cor.mats)){
                warning(paste("Both reals and biased.cor.mats arguments are filled:",
                    "reals argument ignored"))
            }
            if (!is.null(biased.cor.mats)){
                covmats <- invisible(lapply(seq_along(cor.mats), function(i) {
                a <- c((mat2vect(reorg(raw.cors[[i]], outcome.var))[1:nrow(covmats[[i]]), 3])/
                    (mat2vect(reorg(cor.mats[[i]], outcome.var))[1:nrow(covmats[[i]]), 3]))
                    diag(covmats[[i]]) <- (variances(mat2vect(reorg(raw.cors[[i]], outcome.var))[, 
                        3], n.vect[[i]])[1:nrow(covmats[[i]])])/(a^2)
                    covmats[[i]]
                })) 
            } else {
                covmats <- invisible(lapply(seq_along(cor.mats), function(i) {
                a <- c((mat2vect(reorg(raw.cors[[i]], outcome.var))[1:nrow(covmats[[i]]), 3])/
                    (mat2vect(reorg(cor.mats[[i]], outcome.var))[1:nrow(covmats[[i]]), 3]))
                    diag(covmats[[i]]) <- (variances(mat2vect(reorg(raw.cors[[i]], outcome.var))[, 
                        3], n.vect[[i]])[1:nrow(covmats[[i]])])/(a^2)
                    covmats[[i]]
                }))       
            }
        }    
    }
    names(covmats) <- names(cor.mats)
    if (mvmeta.out) {
        covmats <- mvdf(covmats, sep = sep)
        cormats2 <- do.call(rbind, lapply(cor.mats, function(x) {
            x[2:nrow(x), 1]
        }))
        return(list(cormat = cormats2, covmat = covmats))
    }
    class(covmats) <- "asy_cov" 
    covmats
}


#' Prints a asy_cov object
#' 
#' Prints a asy_cov object.
#' 
#' @param x The asy_cov object
#' @param \ldots ignored
#' @method print asy_cov
#' @S3method print asy_cov
print.asy_cov <- function(x, ...) {
	z <- unclass(x)
	if (!is.matrix(z)){
		z <- lapply(z, round, 4)
	} else {
		z <- round(z, 5)
	}
	print(z, ...)
}






