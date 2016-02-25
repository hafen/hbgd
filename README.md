# hbgd

R package for visual and analytical methods for the analysis of longitudinal growth data.

## Installation

```r
options(repos = c(
  CRAN = "http://cran.rstudio.com/",
  tessera = "http://packages.tessera.io",
  BioCsoft = "http://bioconductor.org/packages/3.0/bioc"))
install.packages("hbgd")
```

Note that due to a dependency on [dplyr](https://github.com/hadley/dplyr), which requires the latest version of R, if you are running an older version of R you will have to manually install dplyr first.  Also note that some of the modeling packages that this package depends on have dependencies on Bioconductor.  If the above instructions fail, you may need to install Bioconductor and related packages manually as well.

## Use

See [here](http://hafen.github.io/docs-hbgd/).
