\name{MDexclude}
\alias{MDexclude}
\alias{MDinclude}
\title{Include/Exclude Variables from an MDlist}
\usage{
  MDexclude(meta.obj, vars)

  MDinclude(meta.obj, vars)
}
\arguments{
  \item{meta.obj}{An MDlist (an object from
  \code{read.MD}).}

  \item{vars}{A character vector of the variable to
  include/exclude.}
}
\value{
  Returns an MDlist with just the included variables
  (\code{MDinclude}) or the excluded variables
  (\code{MDexclude}) removed from each study.
}
\description{
  Convience tools to quickly include or exlcude variables
  from all studies of an MDlist object.  Similar
  functionality to \code{subset}.
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat <- read.MD(path)
vars.list <- c("coder", "time", "ee_phi")
MDinclude(dat, vars.list)
MDexclude(dat, vars.list)
}
\seealso{
  \code{\link[base]{subset}}
}

