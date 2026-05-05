# Helper function to create a filtering list for computed files by format

Helper function to create a filtering list for computed files by format

## Usage

``` r
computed_files_filter(
  format_str = c("SINGLE_CELL_EXPERIMENT", "ANN_DATA", "SPATIAL")
)
```

## Arguments

- format_str:

  a string indicating the desired format

## Value

a list suitable for passing as the `filters` argument to
[`get_computed_file_ids()`](https://alexslemonade.github.io/ScPCAr/reference/get_computed_file_ids.md).
