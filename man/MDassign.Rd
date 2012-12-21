\name{MDassign}
\alias{MDassign}
\title{Assign Value to metaDAT List}
\usage{
  MDassign(meta.obj, var, values, NA2NULL = TRUE)
}
\arguments{
  \item{meta.obj}{An MDlist (an object from
  \code{read.MD}).}

  \item{var}{A length one character vector of the variable
  name to assign to.}

  \item{values}{A vector of value(s) to assign to var.}

  \item{NA2NULL}{logical.  If TRUE converts missing (NA) to
  NULL.}
}
\value{
  Returns a MDlist object with the newly assigned values.
}
\description{
  Assign value(s) to a new or existing MDlist variable
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat <- read.MD(path)
MDassign(dat, "new", rnorm(3))
MDassign(dat, "nt", rnorm(3))
MDassign(dat, "new", list(1, 2, 3))
MDassign(dat, "new", list(1, 2, NULL))
MDassign(dat, "new", c(1, 2, NA))
MDassign(dat, "new", c(1, 2, NA), NA2NULL = FALSE)
MDassign(dat, "new", list(NA, mtcars[, 1], mtcars))
}

