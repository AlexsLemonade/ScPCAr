# Add or remove samples and projects in a custom dataset

`add_dataset_samples()` adds the given samples and/or all samples from
the given projects to an existing dataset, keeping the samples already
present. `remove_dataset_samples()` removes the given samples and/or
projects, keeping the remaining samples; any project left with no
samples is dropped from the dataset.

## Usage

``` r
add_dataset_samples(
  dataset,
  samples = NULL,
  projects = NULL,
  include_bulk = FALSE,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
)

remove_dataset_samples(
  dataset,
  samples = NULL,
  projects = NULL,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
)
```

## Arguments

- dataset:

  the dataset UUID string, or a list with an `$id` element.

- samples:

  optional character vector of ScPCA sample IDs to add or remove.

- projects:

  optional character vector of ScPCA project IDs to add or remove; all
  samples from each project are included.

- include_bulk:

  logical; for `add_dataset_samples()`, the `includes_bulk` value to use
  for projects that are newly added to the dataset. Existing projects
  keep their current value. Default is FALSE.

- auth_token:

  an authorization token from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
  Defaults to the `SCPCA_AUTH_TOKEN` environment variable, which
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  sets automatically.

## Value

the dataset ID as a character string (invisibly)

## Details

In both cases the current dataset is fetched, its `data` is modified
locally (taking the union per project and modality when adding, or
dropping the specified IDs when removing), and the updated selection is
sent back via a PUT request. To replace the dataset contents wholesale
instead, use
[`replace_dataset_data()`](https://alexslemonade.github.io/ScPCAr/reference/replace_dataset_data.md).

A dataset that has already been started cannot be modified. Projects
whose single-cell data is merged (SINGLE_CELL = "MERGED") cannot be
modified by sample (though they may be removed wholesale via
`projects`); use
[`replace_dataset_data()`](https://alexslemonade.github.io/ScPCAr/reference/replace_dataset_data.md)
instead.

## Examples

``` r
if (FALSE) { # \dontrun{
add_dataset_samples(ds, samples = "SCPCS000003")
add_dataset_samples(ds, projects = "SCPCP000002")

remove_dataset_samples(ds, samples = "SCPCS000003")
remove_dataset_samples(ds, projects = "SCPCP000002")
} # }
```
