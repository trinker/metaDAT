# metaDAT
====

Tools for data management, reshaping and basic transformations in meta-analysis

## Installation

Currently there isn't a release on [CRAN](http://cran.r-project.org/).


You can, however, download the [zip ball](https://github.com/trinker/metaDAT) or [tar ball](https://github.com/trinker/metaDAT), decompress and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
# install.packages("devtools")

library(devtools)
install_github("metaDAT", "trinker")
```

Windows users currently must install `RCurl` before installing metaDAT.  Use the following short script:

```r
URL <- "http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/2.15/"
install.packages("RCurl", contriburl = URL)
```
## Installing JabRef

[metaDAT](http://trinker.github.com/metaDAT/) utilizes 
[JabRef](http://jabref.sourceforge.net/) as a graphical 
user interface for data entry.  You will need to install a working copy from:

[http://sourceforge.net/projects/jabref/files/jabref/2.9.2/](http://sourceforge.net/projects/jabref/files/jabref/2.9.2/)

## Help
The qdap web page: [trinker.github.com/metaDAT](http://trinker.github.com/metaDAT/)  [improvements to come]    
For the package pdf help manual [click here](https://dl.dropbox.com/u/61803503/metaDAT.pdf).

