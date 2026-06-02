# Replace the contents of an existing custom dataset

Replaces the samples and/or projects in an existing dataset with a new
selection, by sending a PUT request with a freshly built `data` field.
This is a wholesale replacement: the resulting dataset contains exactly
the samples and projects supplied here. To incrementally add or remove
samples while keeping the rest, use
[`add_dataset_samples()`](https://alexslemonade.github.io/ScPCAr/reference/modify_dataset_samples.md)
or
[`remove_dataset_samples()`](https://alexslemonade.github.io/ScPCAr/reference/modify_dataset_samples.md).

## Usage

``` r
replace_dataset_data(
  dataset,
  samples = NULL,
  projects = NULL,
  include_bulk = FALSE,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
)
```

## Arguments

- dataset:

  the dataset UUID string, or a list with an `$id` element.

- samples:

  optional character vector of ScPCA sample IDs (e.g. "SCPCS000001").

- projects:

  optional character vector of ScPCA project IDs (e.g. "SCPCP000001");
  all samples from each project are included.

- include_bulk:

  logical; whether to include bulk RNA-seq files. Default is FALSE.

- auth_token:

  an authorization token from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
  Defaults to the `SCPCA_AUTH_TOKEN` environment variable, which
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  sets automatically.

## Value

the updated dataset detail as a list (invisibly)

## Details

A dataset that has already been started cannot be updated.

## Examples

``` r
if (FALSE) { # \dontrun{
replace_dataset_data(ds, samples = c("SCPCS000001", "SCPCS000002"))
} # }
```
