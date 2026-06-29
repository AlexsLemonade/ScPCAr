#!/usr/bin/env Rscript

# This script renders vignettes that depend on data downloads from the SCPCA API.
# Run this script prior to building the package to pre-compile vignettes.
#
# Usage: Rscript vignettes/precompile.R

uuid_regex <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"

devtools::load_all()
setwd(here::here("vignettes"))

# Get auth token and set it in the environment for knitr to use
ScPCAr::get_auth(email = "scpca@ccdatalab.org", agree = TRUE)


# Pre-compiled vignettes that depend on data downloads
knitr::knit("uncompiled/ScPCAr_pre.Rmd", output = "ScPCAr.Rmd")
