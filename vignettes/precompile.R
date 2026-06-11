#!/usr/bin/env Rscript

# This script renders vignettes that depend on data downloads from the SCPCA API.
# Run this script prior to building the package to pre-compile vignettes.
#
# Set the SCPCA_AUTH_TOKEN environment variable to a valid token before running this script.
#
# Usage: Rscript vignettes/precompile.R

devtools::load_all()
setwd(here::here("vignettes"))

# Get auth token and set it in the environment for knitr to use
ScPCAr::get_auth(email = "scpca@ccdatalab.org", agree = TRUE)


# Pre-compiled vignettes that depend on data downloads
knitr::knit("uncompiled/ScPCAr_pre.Rmd", output = "ScPCAr.Rmd")
