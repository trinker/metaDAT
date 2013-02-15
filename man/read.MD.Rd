\docType{data}
\name{read.MD}
\alias{.exclvars}
\alias{read.MD}
\title{Read in Meta Analysis Data}
\format{chr [1:16] "title" "publisher" "shorttitle" "volume" "number" "pages" "month" "note" "crossref" "keywords" "file" "doi" "url" "comment" ...}
\usage{
  read.MD(file, entry.name = "@META", out.file = NULL,
    rm.coder = FALSE, rm.timestamp = TRUE,
    rm.comment = TRUE, detect.class = TRUE,
    stringsAsFactors = FALSE, exclude.vars = .exclvars)

  .exclvars
}
\arguments{
  \item{file}{The name of the file which the data are to be
  read from.}

  \item{entry.name}{A character string indicating the type
  of entry from the .bib file.  Default is \code{"@META"}.}

  \item{out.file}{A character string naming the file to
  print to.}

  \item{rm.coder}{logical.  If TRUE removes the coder name
  from the meta list.}

  \item{rm.timestamp}{logical.  If TRUE removes the
  timestamp from the MDlist.}

  \item{rm.comment}{logical.  If TRUE removes the ##
  comments from the MDlist.}

  \item{detect.class}{logical.  If TRUE detects the class
  of the character string(s) for each variable.}

  \item{stringsAsFactors}{logical. Should character vectors
  be converted to factors if detect.class is TRUE?}

  \item{exclude.vars}{A character vector of variables to
  exclude.  Default is \code{.exclvars}, a predefined
  vector of exclusion variables.}
}
\value{
  Returns a list structure with information for each study
  (refered as MDlist).
}
\description{
  Read in meta-analysis data from a .bib file created in
  jabRef (may be any text based file).
}
\examples{
path <- system.file("extdata/bibTest.bib", package = "metaDAT")
read.MD(path)
read.MD(path, rm.comment=FALSE)
evars <- c("coder", "time", "ee_phi")
read.MD(path, exclude.vars = evars)
}
\keyword{datasets}

