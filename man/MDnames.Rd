\name{MDnames}
\alias{MDnames}
\title{The Unique Names of a MDList}
\usage{
  MDnames(meta.obj)
}
\arguments{
  \item{meta.obj}{An MDlist (an object from
  \code{read.MD}).}
}
\value{
  Returns the unique names of a metaDAT list.
}
\description{
  Function to get the unique names of an metaDAT list
  object.
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat <- read.MD(path)
MDnames(dat)
}

