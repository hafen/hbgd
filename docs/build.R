knitr::opts_knit$set(root.dir = normalizePath("./docs"))

packagedocs::render_docs(
  code_path = ".",           # location of code directory
  docs_path = "./docs",      # location of docs directory
  package_name = "hbgd",     # name of the package
  main_toc_collapse = TRUE,  # use collapsing toc on main page
  rd_toc_collapse = TRUE,    # use collapsing toc on rd page
  lib_dir = "assets",        # put assets in "assets" directory
  render_main = TRUE,        # render main page
  render_rd = TRUE,          # render rd page
  view_output = TRUE,        # look at the output after render
  rd_index = "rd_index.yaml" # optional path to rd layout yaml
)

# setwd("docs")
# system("R CMD build ../../hbgd")

packagedocs::purl_docs("docs", "docs/code")
