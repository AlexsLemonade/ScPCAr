# ScPCAr

An R package for interacting with the [Single-cell Pediatric Cancer Atlas (ScPCA) Portal](https://scpca.alexslemonade.org) API.

<!-- badges: start -->
[![R-CMD-check](https://github.com/AlexsLemonade/ScPCAr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AlexsLemonade/ScPCAr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

The `ScPCAr` package can be installed from the GitHub repository using the `remotes` package:

```r
remotes::install_github("AlexsLemonade/ScPCAr")
```

## Usage

The package provides functions to get metadata and download data from the ScPCA Portal from within R. 
The code chunk below shows an example of how to authenticate and download a single sample from the portal.

```r
library(ScPCAr)

# First, look at the terms of use
view_terms()

# Get an authentication token for use with the ScPCA Portal
auth_token <- get_auth(email = "your.email@example.com", agree = TRUE)

# Get the sample metadata for a project
sample_metadata <- get_sample_metadata(project_id = "SCPCP000001")

# Download a data for a sample
# this function returns a vector of the downloaded file paths
file_paths <- download_sample(
  sample_id = "SCPCS000001",
  auth_token = auth_token,
  destination = "scpca_data",
  format = "sce"
)
```

For detailed usage instructions, please refer to the [package documentation](https://alexslemonade.github.io/ScPCAr/).
