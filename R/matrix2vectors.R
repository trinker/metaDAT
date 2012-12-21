#' Convert Between Correlation Matrix and Upper Triangle Dataframe  
#' 
#' Tools to convert between a correlation matrix and a dataframe of upper 
#' triangle values and variable components.  The dataframe is more intuitive for 
#' applying functions while the correlation matrix is more intuitive to 
#' visualize.
#' 
#' @aliases matrix2vectors, vectors2matrix
#' @rdname matrix2vectors
#' @param cor.mat A square, symetrical matrix with ones on the diagonal (a 
#' correlation matrix).
#' @param cor.vect A dataframe with the row variables of the correlation matrix 
#' in the first column, the column names in the second column and the 
#' corresponding correlations in the third column.
#' @export
#' @examples
#' (mat <- round(cor(mtcars[, 1:5]), 2))
#' matrix2vectors(mat)
#' cor.vect <- matrix2vectors(round(cor(mtcars[, 1:5]), 2))
#' vectors2matrix(cor.vect)
matrix2vectors <- function(cor.mat) {
    nmscor <- colnames(cor.mat)
    rows <- nmscor[1:(length(nmscor)-1)]
    cols <- nmscor[2:length(nmscor)]
    rowdim <- 1:length(rows)
    row.var <- rows[unlist(lapply(seq_along(rowdim), function(i) rowdim[1:i]))]
    col.var <- rep(cols, 1:length(cols))
    cors <- cor.mat[upper.tri(cor.mat)]
    data.frame(row.var, col.var, cors)
}


#' @rdname matrix2vectors
#' @export
vectors2matrix <- function(cor.vect) {
    dimnms <- unique(c(as.character(cor.vect[, 1]), 
        as.character(cor.vect[, 2])))
    mat <- matrix(NA, length(dimnms), length(dimnms))
    mat[upper.tri(mat)] <- cor.vect[, 3]
    diag(mat) <- 1
    dimnames(mat) <- list(dimnms, dimnms)
    mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
    mat
}
