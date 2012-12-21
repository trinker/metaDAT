\name{MDstr}
\alias{MDstr}
\title{Display the Class of MDlist Variables}
\usage{
  MDstr(meta.obj)
}
\arguments{
  \item{meta.obj}{An MDlist (an object from
  \code{read.MD}).}
}
\description{
  Provides the class of each MDlist variable.  Similar to
  \code{str} though less detailed.
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat <- read.MD(path)
MDstr(dat)
dat2 <- read.MD(path, , stringsAsFactors = TRUE)
MDstr(dat2)
}
\seealso{
  \code{\link[utils]{str}}
}
\keyword{structure}

