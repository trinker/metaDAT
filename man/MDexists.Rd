\name{MDexists}
\alias{MDexists}
\title{Check Variable Existance}
\usage{
  MDexists(meta.obj, vars)
}
\arguments{
  \item{meta.obj}{An MDlist (an object from
  \code{read.MD}).}

  \item{vars}{A character vector of variable names to check
  existance.}
}
\description{
  Logical test to check the existance of variables.
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat <- read.MD(path)
MDexists(dat, "title")
}
\seealso{
  \code{\link[base]{exists}}
}

