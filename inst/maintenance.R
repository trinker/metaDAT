#========================
#staticdocs build version
#========================
#packages
library(highlight); library(qdap); library(staticdocs); library(acc.roxygen2)

#STEP 1: create static doc  
#right now examples are FALSE in the future this will be true
#in the future qdap2 will be the go to source
build_package(package="C:/Users/trinker/GitHub/metaDAT", 
    base_path="C:/Users/trinker/Desktop/metaDAT_dev/", examples = FALSE)

#STEP 2: reshape index
path <- "C:/Users/trinker/Desktop/metaDAT_dev"
path2 <- paste0(path, "/index.html")
rdme <- "C:/Users/trinker/GitHub/metaDAT/inst/extra_statdoc/readme.R"
#extras <- qcv(right.just, coleman_liau, flesch_kincaid, fry, 
#    linsear_write, SMOG, syn, mgsub, adjmat, wc, wfdf, mcsv_w)
readme_statdoc(path2, readme = rdme, file = path2)

#STEP 3: move to trinker.guthub
file <- "C:/Users/trinker/GitHub/trinker.github.com/"
delete(paste0(file, "metaDAT_dev"))
file.copy(path, file, TRUE, TRUE)
delete(path)
#==========================
#staticdocs current version
#==========================
#packages
library(highlight); library(qdap); library(staticdocs); library(acc.roxygen2)

#STEP 1: create static doc  
#right now examples are FALSE in the future this will be true
#in the future qdap2 will be the go to source
build_package(package="C:/Users/trinker/GitHub/metaDAT", 
    base_path="C:/Users/trinker/Desktop/metaDAT/", examples = FALSE)

#STEP 2: reshape index
path <- "C:/Users/trinker/Desktop/metaDAT"
path2 <- paste0(path, "/index.html")
rdme <- "C:/Users/trinker/GitHub/metaDAT/inst/extra_statdoc/readme.R"
#extras <- qcv(right.just, coleman_liau, flesch_kincaid, fry, 
#    linsear_write, SMOG, syn, mgsub, adjmat, wc, wfdf, mcsv_w)
readme_statdoc(path2, readme = rdme, path2)

#STEP 3: move to trinker.guthub
file <- "C:/Users/trinker/GitHub/trinker.github.com/"
delete(paste0(file, "metaDAT"))
file.copy(path, file, TRUE, TRUE)
delete(path)