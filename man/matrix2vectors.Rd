\name{matrix2vectors}
\alias{matrix2vectors}
\alias{matrix2vectors,}
\alias{vectors2matrix}
\title{Convert Between Correlation Matrix and Upper Triangle Dataframe}
\usage{
  matrix2vectors(cor.mat)

  vectors2matrix(cor.vect)
}
\arguments{
  \item{cor.mat}{A square, symetrical matrix with ones on
  the diagonal (a correlation matrix).}

  \item{cor.vect}{A dataframe with the row variables of the
  correlation matrix in the first column, the column names
  in the second column and the corresponding correlations
  in the third column.}
}
\description{
  Tools to convert between a correlation matrix and a
  dataframe of upper triangle values and variable
  components.  The dataframe is more intuitive for applying
  functions while the correlation matrix is more intuitive
  to visualize.
}
\examples{
(mat <- round(cor(mtcars[, 1:5]), 2))
matrix2vectors(mat)
cor.vect <- matrix2vectors(round(cor(mtcars[, 1:5]), 2))
vectors2matrix(cor.vect)
}

