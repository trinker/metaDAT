\name{MDclass}
\alias{MDclass}
\title{Auto Assign Classes}
\usage{
  MDclass(meta.obj, stringsAsFactors = FALSE)
}
\arguments{
  \item{meta.obj}{An MDlist (an object from
  \code{read.MD}).}

  \item{stringsAsFactors}{logical. Should character vectors
  be converted to factors? Default is FALSE.}
}
\value{
  Returns an MDlist structure with appropriate classes.
}
\description{
  Detects the intended class of the like elements of an
  MDlist for each variable across studies.  The function
  only detects numeric or character/factor.
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat <- read.MD(path)
MDclass(dat)
MDclass(dat, stringsAsFactors = TRUE)
}

