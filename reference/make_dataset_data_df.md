# Build the per-sample data frame for a dataset

For each project in the dataset `$data` list, fetches the project's
sample metadata with
[`get_project_samples()`](https://alexslemonade.github.io/ScPCAr/reference/get_project_samples.md)
and keeps only the samples that the dataset includes: For a "regular"
project the IDs listed under `SINGLE_CELL`/`SPATIAL`, and for a merged
project, all of the project's single-cell samples. Each modality flag is
reported only as TRUE for samples that are both included in the dataset
and actually have that modality available:

- `seq_unit` gives the single-cell sequencing unit ("cell" or "nucleus",
  or `NA` when the sample is not included as single-cell)

- `has_spatial` marks spatial inclusion, if requested, for the sample or
  project

- `has_bulk` indicates that the sample is present in the bulk data
  table, if requested for a project.

- `has_cite_seq` and `has_multiplexed` come from the sample records and
  do not depend on the specific request

## Usage

``` r
make_dataset_data_df(data)
```

## Arguments

- data:

  the project-keyed `$data` list from
  [`get_dataset_detail()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_detail.md)

## Value

a data frame with one row per included sample and columns
`scpca_sample_id`, `scpca_project_id`, `seq_unit` (character: "cell",
"nucleus", or `NA`), `has_spatial`, `has_bulk`, `has_cite_seq`, and
`has_multiplexed` (all logical)
