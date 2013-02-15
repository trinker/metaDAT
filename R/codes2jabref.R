#' Create Custom Name Entry Type for Jabref
#'
#' Creates a custom entry type template that can be imported into 
#' \href{http://jabref.sourceforge.net/}{Jabref} to ensure consistency in entry 
#' field names across coders
#'
#' @param required_field Character vecor of items to put in the required field 
#' (it is suggested the user use only one field and leave the optional field 
#' blank).
#' @param optional_field Character vector of items to include in the optional 
#' field.
#' @param file A path or a character string naming the file to print to (if does 
#' not end in .bib or .txt and \code{lazy = TRUE} a .bib will be added to the 
#' path name).
#' @param custom_name The name given to the JabRef custom entry type (default is 
#' META).
#' @param lazy logical.  If TRUE and file does not end in .bib or .txt and 
#' \code{lazy = TRUE} a .bib will be added to the path name.
#' @export
#' @examples
#' codes2jabref(
#'     required_field = c("study", "author", "n.obs", "type"),
#'     optional_field = c("corr.mat", "reliabilities"),
#'     file = NULL,
#'     custom_name = "META_STUDY"
#' )
#' \dontrun{
#' codes2jabref(
#'     required_field = c("study", "author", "n.obs", "type"),
#'     optional_field = c("corr.mat", "reliabilities"),
#'     file = "meta_study",
#'     custom_name = "META_STUDY"
#' )
#' }
codes2jabref <- function(required_field = NULL, optional_field = NULL, 
    file = NULL, custom_name = "META", lazy = TRUE){
    if (is.null(required_field) & is.null(optional_field)) {
       stop("either `required_field` and/or `otional_field` arguments must be supplied")
    }
    if (is.null(required_field)) {
        required_field <- "NULL"
    }
    if (is.null(optional_field)) {
        required_field <- "NULL"
    }
    x <- paste0("@comment{jabref-entrytype: ",
        custom_name, ": req[", paste(required_field, collapse = ";"), "] opt[", 
        paste(optional_field, collapse = ";"), "]}")
    print(x)
    if (!is.null(file)) {
        if (lazy && tools::file_ext(file) != c("txt", "bib")) {
            file <- paste0(file, ".bib")
        }
        cat(x, file = file)
    }
}
