\name{MD_IRR}
\alias{MD_IRR}
\title{Percent Inter-Rater Reliabilitiy}
\usage{
  MD_IRR(meta.obj.1, meta.obj.2,
    exclude.vars = c("author", "ID", "time"),
    cor.var = NULL, comp.var = "coder", file = NULL,
    order.by.comp.var = TRUE)
}
\arguments{
  \item{meta.obj.1}{An MDlist (an object from
  \code{read.MD}) for time 1 (primary coding).}

  \item{meta.obj.2}{An MDlist (an object from
  \code{read.MD}) for time 2 (secondary coding).}

  \item{exclude.vars}{A character vector of variables to
  exclude from reliability check.}

  \item{cor.var}{An optional argument for the MDlist
  variable that is a list of correlation matrices (can
  detect and handle correlation variables that are in
  character correlation triangle format).}

  \item{comp.var}{The variable to compare for discrepencies
  between the two MDlists. Default is "coder".  This
  argument is particularly useful when there are more than
  two coders.}

  \item{file}{Optional connection, or a character string
  naming the file to print to.}

  \item{order.by.comp.var}{logical.  If TRUE orders the
  study disagreements by the comparison variable.}
}
\value{
  Returns a list: \item{Variable_Agreement_Rate}{A
  dataframe of percentage agreement/disagreement for each
  variable.  Final row is overall agreement rate.}
  \item{Disagreements}{A list of disagreements of study
  variables between comparison groups (time 1 and time 2)
  per study.  If correlations are being compared the
  function outputs an agreement matrix.  Agreement is
  marked as \code{-} and disagreement as \code{X}.}
}
\description{
  Percent inter-rater reliabilitiy by variable.
}
\note{
  This function assumes double coding, that is all studies
  have been coded by two different coders.
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat1 <- read.MD(path)
path2 <- system.file("extdata/bibTest2.bib", package = "metaDAT")
dat2 <- read.MD(path2)
MD_IRR(dat1, dat2, cor.var = "correlation_triangle")
# MD_IRR(dat1, dat2, cor.var = "correlation_triangle", file = "foo.txt")
# unlink("foo.txt", recursive = TRUE, force = FALSE) #delete previous file
}
\keyword{inter-rater}
\keyword{reliability}

