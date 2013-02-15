\name{codes2jabref}
\alias{codes2jabref}
\title{Create Custom Name Entry Type for Jabref}
\usage{
  codes2jabref(required_field = NULL,
    optional_field = NULL, file = NULL,
    custom_name = "META", lazy = TRUE)
}
\arguments{
  \item{required_field}{Character vecor of items to put in
  the required field (it is suggested the user use only one
  field and leave the optional field blank).}

  \item{optional_field}{Character vector of items to
  include in the optional field.}

  \item{file}{A path or a character string naming the file
  to print to (if does not end in .bib or .txt and
  \code{lazy = TRUE} a .bib will be added to the path
  name).}

  \item{custom_name}{The name given to the JabRef custom
  entry type (default is META).}

  \item{lazy}{logical.  If TRUE and file does not end in
  .bib or .txt and \code{lazy = TRUE} a .bib will be added
  to the path name.}
}
\description{
  Creates a custom entry type template that can be imported
  into \href{http://jabref.sourceforge.net/}{Jabref} to
  ensure consistency in entry field names across coders
}
\examples{
codes2jabref(
    required_field = c("study", "author", "n.obs", "type"),
    optional_field = c("corr.mat", "reliabilities"),
    file = NULL,
    custom_name = "META_STUDY"
)
\dontrun{
codes2jabref(
    required_field = c("study", "author", "n.obs", "type"),
    optional_field = c("corr.mat", "reliabilities"),
    file = "meta_study",
    custom_name = "META_STUDY"
)
}
}

