# Download a sample's data files from the ScPCA Portal

This function downloads the data files for a specified sample from the
ScPCA Portal. The downloaded files are saved in a subdirectory of the
specified path, named by the base filename of the downloaded zip file,
which includes the sample ID, modality, format, and date.

## Usage

``` r
download_sample(
  sample_id,
  destination = "scpca_data",
  format = "sce",
  overwrite = FALSE,
  redownload = FALSE,
  quiet = FALSE,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
)
```

## Arguments

- sample_id:

  The ScPCA sample ID (e.g. "SCPCS000001")

- destination:

  The path to the directory where the unzipped file directory should be
  saved. Default is "scpca_data".

- format:

  The desired file format, either "sce" (SingleCellExperiment),
  "anndata" (AnnData/H5AD), or "spatial" (for spatial data in Space
  Ranger format). Default is "sce".

- overwrite:

  Whether to overwrite files in existing directories if they already
  exist. Note that files in existing directories that do not have the
  same name as one of the downloaded files will not be deleted. Default
  is FALSE.

- redownload:

  Whether to re-download if files from the same sample and format
  already exist. If FALSE, existing files will be returned. Default is
  FALSE.

- quiet:

  Whether to suppress download progress messages. Default is FALSE.

- auth_token:

  An authorization token from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
  Defaults to the `SCPCA_AUTH_TOKEN` environment variable, which
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  sets automatically.

## Value

a vector of file paths for the downloaded files (invisibly)

## Details

For single-cell and single-nuclei data, files can be downloaded in
either SingleCellExperiment ("sce") or AnnData ("anndata") format, and
all downloads include the unfiltered, filtered, and processed data
objects, as well as associated metadata and QC files.

Spatial data, if present, can be downloaded in Space Ranger format using
the "spatial" format option.

The function returns a vector of file paths for the downloaded files
(invisibly).

Note that downloading data requires an authorization token, which can be
obtained using the
[`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
function.

## Examples

``` r
if (FALSE) { # \dontrun{
# get_auth() stores the token in SCPCA_AUTH_TOKEN, so downloads pick it up automatically
get_auth("your.email@example.com", agree = TRUE)
# Then ask for a sample download
download_sample("SCPCS000001", destination = "scpca_data", format = "sce")

# Downloading in AnnData format
download_sample("SCPCS000001", destination = "scpca_data", format = "anndata")
} # }
```
