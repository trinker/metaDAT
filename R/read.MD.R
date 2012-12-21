#' Read in Meta Analysis Data
#' 
#' Read in meta-analysis data from a .bib file created in jabRef (may be any 
#' text based file).
#' 
#' @param file The name of the file which the data are to be read from.
#' @param entry.name A character string indicating the type of entry from the 
#' .bib file.  Default is \code{"@@META"}.
#' @param out.file A character string naming the file to print to.
#' @param rm.coder logical.  If TRUE removes the coder name from the meta list.
#' @param rm.timestamp logical.  If TRUE removes the timestamp from the MDlist.
#' @param rm.comment logical.  If TRUE removes the ## comments from the MDlist.
#' @param detect.class logical.  If TRUE detects the class of the character 
#' string(s) for each variable.
#' @param stringsAsFactors logical. Should character vectors be converted to 
#' factors if detect.class is TRUE?
#' @param  exclude.vars A character vector of variables to exclude.  Default is 
#' \code{.exclvars}, a predefined vector of exclusion variables.
#' @return Returns a list structure with information for each study (refered 
#' as MDlist).
#' @export
#' @examples
#' path <- system.file("extdata/bibTest.bib", package = "metaDAT")
#' read.MD(path)
#' read.MD(path, rm.comment=FALSE)
#' evars <- c("coder", "time", "ee_phi")
#' read.MD(path, exclude.vars = evars)
read.MD <- function(file, entry.name = "@META", out.file = NULL, 
	rm.coder = FALSE, rm.timestamp = TRUE, rm.comment = TRUE, 
    detect.class = TRUE, stringsAsFactors = FALSE, exclude.vars = .exclvars) {
    input <- readLines(file)
    input <- input[!substring(input, 1, 5) %in% c("@comm", "")]
    if (rm.timestamp) {
        input <- input[!grepl("timestamp =", input)]
    }
    if (rm.coder) {
        input <- input[!grepl("owner =", input)]
    } else {
    	input <- gsub("owner =", "coder =", input)
    }
    metalines <- which(substring(input, 1, 5) == entry.name)
    input <- input[metalines[1]:length(input)]
    metalines <- which(substring(input, 1, 5) == entry.name)
    input[metalines] <- gsub(",",  "", input[metalines])    
    input[metalines] <- gsub(paste0(entry.name, "{"),  "", input[metalines], 
        fixed = TRUE)  
    input[length(input)] <- input[metalines[-1] - 1] <- "),"  
    input[metalines] <- paste(input[metalines], "= list(")
    input[metalines[-1]-2] <- substring(input[metalines[-1]-2], 1, 
        nchar(input[metalines[-1]-2]) -1)
    studies <- gsub("(.*?)( =.*)", "\\1", input[metalines])
    input[metalines] <- paste(input[metalines], paste0("ID ={", studies, "},"))
    input[length(input) -1] <- substring(input[length(input) -1], 1, 
        nchar(input[length(input) -1]) -1)    
    input <- gsub("{",  'c("', input, fixed = TRUE) 
    input <- gsub("}",  '")', input, fixed = TRUE) 
    input[length(input)] <- ")"
    input <- input[!input %in% "\t"]
    input <- gsub("\t", "  ", input, fixed = TRUE)
    input <- c("LIST <- list(", paste0(paste(rep(" ", 2), collapse=""), 
        input), ")")
    input <- paste0(input, "\n")
    cat(input, file="temp_metaDAT.txt")
    source("temp_metaDAT.txt", local =TRUE)
    LIST <- rapply(LIST, function(x){gsub("\n", "", x)}, how = "list")
    LIST <- rapply(LIST, function(x){gsub("\\s+", " ", x)}, how = "list")
    if (!is.null(out.file)) {
        file.rename("temp_metaDAT.txt", out.file)
    }
    unlink("temp_metaDAT.txt", recursive = TRUE, force = FALSE)
    if (rm.comment) {
        LIST <- rapply(LIST, function(x){
            gsub("(.*?)(##.*)", "\\1", x)
        }, how = "list")
    }
    LIST <- rapply(LIST, function(x){gsub("^\\s+|\\s+$", "", x)}, how = "list")
    if (detect.class) {
        LIST <- MDclass(LIST, stringsAsFactors = stringsAsFactors)	
    }
    if (!is.null(exclude.vars)) {
    	LIST <- MDexclude(LIST, exclude.vars)
    }
    LIST
}


#' @rdname read.MD
#' @export
.exclvars <- c("title", "publisher", "shorttitle", "volume", "number", 
			   "pages", "month", "note", "crossref", "keywords", "file", "doi", 
			   "url", "comment", "abstract", "review")