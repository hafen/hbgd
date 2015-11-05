# hbgd

R package for visual and analytical methods for the analysis of longitudinal growth data.

## Installation

```r
options(repos = c(tessera = "http://packages.tessera.io", getOption("repos")))
install.packages("hbgd")
```

Note that due to a dependency on [dplyr](https://github.com/hadley/dplyr), which requires the latest version of R, if you are running an older version of R you will have to manually install dplyr first.

Alternatively, you can install directly from github:

```r
# dependencies on github
devtools::install_github("tesseradata/datadr")
devtools::install_github("tesseradata/trelliscope")
devtools::install_github("bokeh/rbokeh@dev")

devtools::install_github("hafen/hbgd")
```

Docs coming soon...
