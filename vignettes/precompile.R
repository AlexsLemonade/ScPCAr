#!/usr/bin/env Rscript

# This script renders vignettes that depend on data downloads from the SCPCA API.
# Run this script prior to building the package to pre-compile vignettes.
#
# Usage: Rscript vignettes/precompile.R


devtools::load_all()

# Pre-compiled vignettes that depend on data downloads
unlink(here::here("vignettes/scpca_data"), recursive = TRUE)
knitr::knit(here::here("vignettes/ScPCAr.Rmd.orig"), output = here::here("vignettes/ScPCAr.Rmd"))

# move figures
unlink(here::here("vignettes/figure"), recursive = TRUE)
file.rename(here::here("figure"), here::here("vignettes/figure"))
