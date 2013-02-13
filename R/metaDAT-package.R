#' metaDAT: Tools for data management, reshaping and basic transformations in 
#' meta-analysis
#'
#' metaDAT is intended to provide a frame work for meta analysis that 
#' uses a list structure rather than the traditional dataframe data storage.  In a 
#' meta-analysis with correlated/dependent data each study may have several effect 
#' sizes.  This causes the problem of more effect sizes than moderators resulting 
#' in the need to replicate these variables in multiple rows.  This approach utilize 
#' JabRef (a refrence management tool) as the input program and R as the import system.  
#' This approach means less repetition of moderator variables in the data frame and 
#' allows for importing a correlation matrix rather than a vector of correlations 
#' through a list approach rather than the traditional dataframe.  This will lead 
#' to more efficient analysis, less error in input and more coherant structure to 
#' the data.  
#' 
#' It is intednded that all data is input directly into JabRef through a user 
#' defined entry.  JabRef can be obtained from \url{http://jabref.sourceforge.net/}.  
#' After JabRef has been installed a custom entry type must be created as described 
#' in this link: 
#' \url{http://jabref.sourceforge.net/help/CustomEntriesHelp.php}.  
#' The custom entry can be named whatever the user desires but the default read in of 
#' the read.MD function is meta, thus it is advisable to follow this convention.  
#' The variables for the meta-analysis are entered in as custom fields.  These will 
#' be the variables stored within the list structure (MDlist) in the metaDAT package.  
#' 
#' After the data has been entered metaDAT provides a numer of functions to prepare 
#' the data into a structure for analysis provided my any of the excellent meta 
#' analysis packages available as an add on package.  
#'
#' @docType package
#' @name metaDAT
#' @import RCurl testthat
#' @aliases metaDAT package-metaDAT
NULL

