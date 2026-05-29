# Get computed file ids from a sample info list, optionally filtered by criteria

Get computed file ids from a sample info list, optionally filtered by
criteria

## Usage

``` r
get_computed_file_ids(info_list, filters = list())
```

## Arguments

- info_list:

  A list object that includes a "computed_files" element, such as
  returned by
  [`get_sample_info()`](https://alexslemonade.github.io/ScPCAr/reference/get_sample_info.md)
  or
  [`get_project_info()`](https://alexslemonade.github.io/ScPCAr/reference/get_project_info.md)

- filters:

  A named list of filtering criteria, where names are fields in the
  computed_files objects, and values are the desired values to match.
  Values can be negated by prefixing with "!". For example, to get all
  non-spatial computed files in SingleCellExperiment format, use:
  list(format = "SINGLE_CELL_EXPERIMENT", modality = "!SPATIAL").

## Value

a character vector of computed file ids matching the filtering criteria
