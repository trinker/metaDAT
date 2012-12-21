#' URL Delivered Documents
#' 
#' A convenience function for passing new data sets to users in the future.
#' 
#' @param \ldots Files for download.
#' @param url The url to pass information from.
#' @return Downloads a file to the users working directory.
#' @export
url_dl <-
function(..., url = "http://dl.dropbox.com/u/61803503/") {
    mf <- match.call(expand.dots = FALSE)
    payload <- as.character(mf[[2]])
    FUN <- function(x, url) {
        bin <- getBinaryURL(paste0(url, x), ssl.verifypeer=FALSE)  
        con <- file(x, open = "wb")
        writeBin(bin, con)
        close(con)
        message(noquote(paste(x, "read into", getwd())))
    }
    invisible(lapply(payload, function(z) FUN(x = z, url = url)))
}

#' @export