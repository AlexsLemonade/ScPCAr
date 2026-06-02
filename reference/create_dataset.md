# Create a custom dataset on the ScPCA Portal

Creates a new user dataset without starting processing. The returned
list includes the dataset `$id` along with its current contents and
status.

## Usage

``` r
create_dataset(
  auth_token,
  format = "sce",
  samples = NULL,
  projects = NULL,
  include_bulk = FALSE,
  email = NULL
)
```

## Arguments

- auth_token:

  an authorization token obtained from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)

- format:

  the desired file format: "sce" (SingleCellExperiment, default) or
  "anndata" (AnnData/H5AD). Spatial data is not a valid format option
  here; spatial samples are always returned in Space Ranger format.

- samples:

  optional character vector of ScPCA sample IDs (e.g. "SCPCS000001")

- projects:

  optional character vector of ScPCA project IDs (e.g. "SCPCP000001");
  all samples from each project are included

- include_bulk:

  logical; whether to include bulk RNA-seq files. Default is FALSE.

- email:

  optional email address for download notification

## Value

the API response as a list (invisibly), including the dataset `$id`

## Examples

``` r
if (FALSE) { # \dontrun{
token <- get_auth("user@example.com", agree = TRUE)
ds <- create_dataset(
  auth_token = token,
  samples = c("SCPCS000001", "SCPCS000002")
)
ds$id
} # }
```
