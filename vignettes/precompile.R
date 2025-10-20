#!/usr/bin/env Rscript

# This script renders vignettes that depend on data downloads from the SCPCA API.
# Run this script prior to building the package to pre-compile vignettes.
#
# Usage: Rscript vignettes/precompile.R


devtools::load_all()
setwd(here::here("vignettes"))

# Pre-compiled vignettes that depend on data downloads
knitr::knit("ScPCAr.Rmd.orig", output = "ScPCAr.Rmd")
