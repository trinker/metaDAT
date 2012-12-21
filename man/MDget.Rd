\name{MDget}
\alias{MDget}
\title{Extract Meta Variables}
\usage{
  MDget(meta.obj, vars, s.names = FALSE, by.study = FALSE)
}
\arguments{
  \item{meta.obj}{An MDlist (an object from
  \code{read.MD}).}

  \item{vars}{A character vector of variable names to
  extract.}

  \item{s.names}{logical.  If TRUE attempts to name the
  extracted vector/list elements with study names.}

  \item{by.study}{logical.  If TRUE orders the variables by
  study rather than by variable.}
}
\description{
  Extract variables from each study for analysis.
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat <- read.MD(path)
MDget(dat, c("nt"))
MDget(dat, c("title", "nt"), TRUE)
MDget(dat, c("title", "nt"))
MDget(dat, "grade_level")
MDget(dat, "cor_mats")
MDget(dat, "cor_mats", s.names = TRUE)
dat <- MDapply(dat, "correlation_triangle", MDlist2matrix, "cor_mats")
MDget(dat, c("title", "nt", "cor_mats"))
MDget(dat, c("grade_level", "nt", "cor_mats"), by.study=TRUE)
MDget(dat, c("grade_level", "nt", "cor_mats"), s.names = TRUE, by.study=TRUE)
}
\seealso{
  \code{\link[base]{get}}
}

