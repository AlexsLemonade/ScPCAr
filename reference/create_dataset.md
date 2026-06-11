# Create a custom dataset on the ScPCA Portal

Creates a new user dataset without starting processing. Returns the new
dataset's ID (invisibly), which you can pass to the other dataset
functions such as
[`get_dataset_info()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_info.md),
[`add_dataset_samples()`](https://alexslemonade.github.io/ScPCAr/reference/modify_dataset_samples.md),
and
[`start_dataset_processing()`](https://alexslemonade.github.io/ScPCAr/reference/start_dataset_processing.md).

## Usage

``` r
create_dataset(
  samples = NULL,
  projects = NULL,
  format = "sce",
  include_bulk = FALSE,
  email = NULL,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
)
```

## Arguments

- samples:

  optional character vector of ScPCA sample IDs (e.g. "SCPCS000001")

- projects:

  optional character vector of ScPCA project IDs (e.g. "SCPCP000001");
  all samples from each project are included

- format:

  the desired file format: "sce" (SingleCellExperiment, default) or
  "anndata" (AnnData/H5AD). Spatial data is not a valid format option
  here; spatial samples are always returned in Space Ranger format.

- include_bulk:

  logical; whether to include bulk RNA-seq files. Default is FALSE.

- email:

  optional email address for download notification

- auth_token:

  an authorization token from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
  Defaults to the `SCPCA_AUTH_TOKEN` environment variable, which
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  sets automatically.

## Value

the dataset ID as a character string (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
token <- get_auth("user@example.com", agree = TRUE)
ds_id <- create_dataset(
  auth_token = token,
  samples = c("SCPCS000001", "SCPCS000002")
)
ds_id
} # }
```
