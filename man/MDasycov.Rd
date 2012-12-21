\name{MDasycov}
\alias{MDasycov}
\title{Asymptotic Variances/Covariance}
\usage{
  MDasycov(cor.mats, n.vect, outcome.var, reals = NULL,
    fishersz = FALSE, mvmeta.out = TRUE,
    biased.cor.mats = NULL, sep = "_x_")
}
\arguments{
  \item{cor.mats}{A list of correlation matrices.  If
  unattenuated supply the list biased correlations to the
  \code{biased.cor.mats} argument.  If biased optionally
  supply a dataframe of reliabilities to the \code{reals}
  argument.}

  \item{n.vect}{A vector of sample sizes corresponding to
  the studies in the list of \code{cor.mats}.}

  \item{outcome.var}{The name of the outcome variable the
  other measures correlate with.}

  \item{reals}{A matrix/data.frame of reliabilities.  The
  column names must match the column/rownames of the
  correlation matrices exactly. (order does not matter).}

  \item{fishersz}{logical.  If TRUE uses Fisher's z
  transformation.}

  \item{mvmeta.out}{logical.  If TRUE outputs a list with
  the correlations and variances/covariances in a format
  that can be used by the \code{mvmeta} package.}

  \item{biased.cor.mats}{A list of biased correlation
  matrices supplied if unattentuated correlations are
  passed to the \code{cor.mats} argument.}

  \item{sep}{A character string to separate the variable
  names.}
}
\value{
  Returns either a list of matrices of asymptotic
  covariances/variances of the correlation matrices or (if
  \code{mvmeta.out} is TRUE) a list of a matrix of
  correlations in the form selected (raw, unattenuated or
  Fishers z transformed) and matrix of
  variances/covariances with studies as rownames that can
  be passed to \code{mvmeta} from the \code{mvmeta}
  package.
}
\description{
  Produce asymptotic variances/covariance with optional
  correction for attentuation and Fisher's z
  transformation.
}
\details{
  Uses Olkin and Siotani's (1976) formulas:
  \deqn{Var(r_{ist})\approx
  \frac{(1-\rho_{ist}^{2})^2}{n_i}} \deqn{Cov(r_{ist},
  r_{iuv})= [.5\rho_{ist}\rho_{iuv}(\rho_{isu}^2 +
  \rho_{isv}^2 + \rho_{itu}^2 + \rho_{itv}^2) +
  \rho_{isu}\rho_{itv}+ \rho_{isv}\rho_{itu}-}
  \deqn{(\rho_{ist}\rho_{isu}\rho_{isv} +
  \rho_{its}\rho_{itu}\rho_{itv}) +
  \rho_{ius}\rho_{iut}\rho_{iuv} +
  \rho_{ivs}\rho_{ivt}\rho_{ivu}]/n_i}

  Unattenuated Variances Formula (Borenstein, Hedges,
  Higgins & Rothstein, 2009):

  \deqn{V_{r}^u = \frac{Var(r_{ist})}{\rho/\rho^u}}

  Where: \cr \eqn{\rho} is the raw correlation \cr
  \eqn{\rho^u} is the unattenuated correlation \cr

  Fisher's z approach also requires:

  \deqn{Var(z_{ist})\approx \frac{1}{n_i - 3}}
  \deqn{Cov(z_{ist}, z_{iuv})=
  \frac{\sigma_{ist,uv}}{(1-\rho_{ist}^2)(1-\rho_{iuv}^2)}}
  If a realibility is missing (\code{NA}) 1 will be used as
  a conservative estimate.
}
\note{
  Reliability column names must match exactly(including
  case) the row/column names of the correlation matrices.
  If needed add a column of \code{NA}s.
}
\examples{
#generating data sets
set.seed(10)
MAT <- function(n = 5) {
    x <- matrix(round(rnorm(n^2, 0, .3), 2), n)
    dimnames(x) <- list(LETTERS[1:n], LETTERS[1:n])
    diag(x) <-1
    x[upper.tri(x)] <- t(x)[upper.tri(x)]
    x
}
cor.mats1 <- lapply(1:5, function(i) MAT(4))
names(cor.mats1) <- paste0("study_", 1:length(cor.mats1))
cor.mats2 <- lapply(1:3, function(i) MAT())
names(cor.mats2) <- paste0("study_", 1:length(cor.mats2))
n1 <- sample(40:150, length(cor.mats1), TRUE)
n2 <- sample(40:150, length(cor.mats2), TRUE)
reliabilities <- matrix(round(rnorm(20, .7, .1), 2), ncol = 4)
dimnames(reliabilities) <- list(names(cor.mats1), LETTERS[1:4])

#Using MDasycov
MDasycov(cor.mats1, n1, outcome.var = "A", reals = reliabilities)
MDasycov(cor.mats1, n1, outcome.var = "D")
MDasycov(cor.mats1, n1, outcome.var = "C")
MDasycov(cor.mats2, n2, outcome.var = "B")
MDasycov(cor.mats2, n2, outcome.var = "E", mvmeta.out = FALSE)
MDasycov(cor.mats2, n2, outcome.var = "E", fishersz = TRUE)

#With the mvmeta package
\dontrun{
mvmDAT <- MDasycov(cor.mats1, n1, outcome.var = "A", reals = reliabilities)
library(mvmeta)
mvmeta(mvmDAT[[1]], mvmDAT[[2]])
}
}
\references{
  Olkin, I., & Siotani, M. (1976) Asymptotic distribution
  of functions of a correlation matrix. In Essays in
  Probability and Statistics Chapter 16 (S. Ikeda, ed.)
  235-251. Shinko Tsusho, Tokyo.

  Borenstein, M., Hedges, L. V., Higgins, J. P. T., &
  Rothstein, H. R. (2009). Introduction to meta- analysis
  (1st ed.). West Sussex, UK: Wiley.

  Becker, B. J. (2000).  Multivariate meta-analysis.  In H.
  A. Tinsley & S. Brown, (Eds.).  Handbook of applied
  multivariate statistics and mathematical modeling.  San
  Diego: Academic Press.
}
\seealso{
  \code{\link[metaDAT]{MDfishersz}},
  \code{\link[metaDAT]{MDunatten}},
  \code{\link[mvmeta]{mvmeta}}
}
\keyword{asymptotic}
\keyword{covariance}

