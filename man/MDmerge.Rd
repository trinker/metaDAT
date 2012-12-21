\name{MDmerge}
\alias{MDmerge}
\title{Merge Two or More MDlists}
\usage{
  MDmerge(..., id.vars = NULL, sep = "_",
    detect.class = TRUE, stringsAsFactors = FALSE)
}
\arguments{
  \item{\ldots}{MDlists (objects from \code{read.MD}) to be
  coerced to one.}

  \item{id.vars}{Specifications of the common variable(s).
  Default is NULL.}

  \item{sep}{A character string to separate the MDlist
  merged study names by + identifiers (id.vars).}

  \item{detect.class}{logical.  If TRUE detects the class
  of the character string(s) for each variable.}

  \item{stringsAsFactors}{logical. Should character vectors
  be converted to factors if detect.class is TRUE?}
}
\value{
  Returns a merged MDlist object.
}
\description{
  Merge two or more MDlists.
}
\note{
  General use is for merging multiple coders with different
  studies together into one MDlist.  This is not
  recommended for combing the same studies coded by two
  different coders (double coded).  Instead keep two
  different MDlist data sets for reliability checking.
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
dat1 <- read.MD(path)
path2 <- system.file("extdata/bibTest3.bib", package = "metaDAT")
dat2 <- read.MD(path2)
datMerged <- MDmerge(dat2, dat1)
datMerged
MDstr(datMerged)
names(datMerged)
MDmerge(dat2, dat1, id.vars = c("coder", "time"), sep = "\%")
}
\seealso{
  \code{\link[base]{merge}}
}

