\name{MDlist2matrix}
\alias{MDlist2matrix}
\title{List of Correlations to Matrix}
\usage{
  MDlist2matrix(x)
}
\arguments{
  \item{x}{A named list of correlations (see example
  below).}
}
\value{
  Retruns a correlation matrix with named dimensions.
}
\description{
  Convert a named list of correlations to a correlation
  matrix with named dimensions.
}
\examples{
x1 <- list(disc=c(.2, .4, .2), EE=c(.67, .54), PA=c(.86), DP=c())
MDlist2matrix(x1)

x2 <- list(disc=c(.6), BO=c())
MDlist2matrix(x2)

x3 <- list(disc=c(), EE=c(.2), PA=c(.4, .67), DP=c( .2, .54, .86))
MDlist2matrix(x3)

x4 <- list(disc=c(.2, .4, .2), EE=c(), PA=c(), DP=c())
MDlist2matrix(x4)

x5 <- list(disc=c(), EE=c(.2), PA=c(.4), DP=c(.2))
MDlist2matrix(x5)

\dontrun{
x6 <- list(disc=c(.2), EE=c(.2), PA=c(.4), DP=c(.2))
MDlist2matrix(x6) #error expected
}

path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat <- read.MD(path)
MDapply(dat, "correlation_triangle", MDlist2matrix, "cor_mats")
}

