# metaDAT
====

Tools for data management in meta-analysis

## Installation

Currently there isn't a release on [CRAN](http://cran.r-project.org/).


You can, however, download the [zip ball](https://github.com/trinker/) or [tar ball](https://github.com/trinker/), decompress and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
# install.packages("devtools")

library(devtools)
install_bitbucket("metaDAT", "trinker")
```

Windows users currently must install `RCurl` before installing metaDAT.  Use the following short script:

```r
URL <- "http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/2.15/"
install.packages("XML", contriburl = URL)
install.packages("RCurl", contriburl = URL)
```

