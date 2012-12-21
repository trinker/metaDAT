\name{MDapply}
\alias{MDapply}
\title{Apply Function to Elements of a MDlist}
\usage{
  MDapply(meta.obj, var, FUN, newvar = NULL)
}
\arguments{
  \item{meta.obj}{An MDlist (an object from
  \code{read.MD}).}

  \item{var}{A length one character vector of the variable
  name to grab.}

  \item{FUN}{A function that takes a single argument.}

  \item{newvar}{A length one character vector of the
  variable name to assign to.}
}
\value{
  Returns a metaDAT list object with the newly assigned
  values.
}
\description{
  A convienence wrapper for \code{MDget} and optionally
  \code{MDassign}. For more complicated needs (i.e. working
  with more than one variable or pieces of a variable) use
  \code{MDget} to retrieve variable(s) and then lapply with
  select functions. Finally, reassign with \code{MDassign}.
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat <- read.MD(path)
dat <- MDapply(dat, "correlation_triangle", MDlist2matrix, "cor_mats")
MDapply(dat, "correlation_triangle", MDlist2matrix)
mean2 <- function(x) mean(unlist(x), na.rm = TRUE)
MDapply(dat, "cor_mats", mean2)
}

