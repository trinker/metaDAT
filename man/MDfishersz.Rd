\name{MDfishersz}
\alias{MDfishersz}
\title{Fisher's z Transformation of Correlation Matrices}
\usage{
  MDfishersz(cor.mats)
}
\arguments{
  \item{cor.mats}{A list of correlation matrices.}
}
\value{
  Returns a list of correlation matrices with Fisher's z
  transformation applied.
}
\description{
  Stabalizes the variances of correlations.  Operates on a
  list of correlation matrices.
}
\details{
  Uses Fisher's (1915) transformation: \deqn{{z}' = .5ln
  \left(\frac{1 + r}{1 - r}\right)}
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat <- read.MD(path)
correlations <- MDapply(dat, "correlation_triangle", MDlist2matrix)
MDfishersz(correlations)
#reassign
dat <- MDassign(dat, "fishz.cor", MDfishersz(correlations))
dat

#In less steps:
fishFUN <- function(x) MDfishersz(MDlist2matrix(x))
MDapply(dat, "correlation_triangle", fishFUN, newvar = "fishz.cor")
}
\references{
  Fisher, R. A. (1915) Frequency distribution of the values
  of the correlation coefficient in samples from an
  indefinitely large population. Biometrika, Vol. 10, pp.
  507-521.
}
\keyword{attentuation}

