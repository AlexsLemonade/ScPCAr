# Download a project's data files from the ScPCA Portal

Download a project's data files from the ScPCA Portal

## Usage

``` r
download_project(
  project_id,
  destination = "scpca_data",
  format = "sce",
  merged = FALSE,
  include_multiplexed = NULL,
  overwrite = FALSE,
  redownload = FALSE,
  quiet = FALSE,
  unzip = TRUE,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
)
```

## Arguments

- project_id:

  The ScPCA project ID (e.g. "SCPCP000001")

- destination:

  The path to the directory where the unzipped file directory should be
  saved. Default is "scpca_data".

- format:

  The desired file format, either "sce" (SingleCellExperiment),
  "anndata" (AnnData/H5AD), or "spatial" (for spatial data in Space
  Ranger format). Default is "sce".

- merged:

  Download merged data files, if available. Default is FALSE.

- include_multiplexed:

  Include multiplexed samples, if available. Default is TRUE for
  SingleCellExperiment and FALSE for AnnData and spatial samples, where
  multiplexed data are not available.

- overwrite:

  Whether to overwrite files in existing directories if they already
  exist. Note that files in existing directories that do not have the
  same name as one of the downloaded files will not be deleted. Default
  is FALSE.

- redownload:

  Whether to re-download if files from the same project and format
  already exist. If FALSE, existing files will be returned. Default is
  FALSE.

- quiet:

  Whether to suppress download progress messages. Default is FALSE.

- unzip:

  Whether to unzip the downloaded file. Default is TRUE. When FALSE, the
  zip file is saved directly to `destination` and its path is returned.

- auth_token:

  An authorization token from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
  Defaults to the `SCPCA_AUTH_TOKEN` environment variable, which
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  sets automatically.

## Value

a vector of file paths for the downloaded files (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
# get_auth() stores the token in SCPCA_AUTH_TOKEN, so downloads pick it up automatically
get_auth("your.email@example.com", agree = TRUE)
# Then ask for a project download
download_project("SCPCP000001", destination = "scpca_data", format = "sce")

# Downloading merged files in AnnData format
download_project(
  "SCPCP000001",
  destination = "scpca_data",
  format = "anndata",
  merged = TRUE
)
} # }
```
