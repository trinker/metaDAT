\name{mvdf}
\alias{mvdf}
\title{Reorder Matrix for Analysis}
\usage{
  mvdf(cor.mats, sep = "_")
}
\arguments{
  \item{cor.mats}{A square, symetrical matrix with ones on
  the diagonal (a correlation matrix).}

  \item{sep}{A character string to separate the variable
  names.}
}
\description{
  \code{mvdf} is not intended for general use. The function
  takes a list of correlations matrices, grabs the lower
  triangles and diagonals and unlists by columns.
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat <- read.MD(path)
dat <- MDapply(dat, "correlation_triangle", MDlist2matrix, "cor_mats")
mats <- MDget(dat, "cor_mats", s.names=TRUE)
mvdf(mats)
mvdf(mats, "_x_")
}

