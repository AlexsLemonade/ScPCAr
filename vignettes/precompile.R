#!/usr/bin/env Rscript

# This script renders vignettes that depend on data downloads from the SCPCA API.
# Run this script prior to building the package to pre-compile vignettes.
#
# Set the SCPCA_AUTH_TOKEN environment variable to a valid token before running this script.
#
# Usage: Rscript vignettes/precompile.R

uuid_regex <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"

stopifnot(
  "SCPCA_AUTH_TOKEN environment variable must be set and a UUID" = grepl(
    uuid_regex,
    Sys.getenv("SCPCA_AUTH_TOKEN")
  )
)

devtools::load_all()
setwd(here::here("vignettes"))

# Pre-compiled vignettes that depend on data downloads
knitr::knit("ScPCAr.Rmd.orig", output = "ScPCAr.Rmd")
