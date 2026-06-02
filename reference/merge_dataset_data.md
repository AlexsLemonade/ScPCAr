# Merge additional dataset data into an existing dataset data structure

Combines two dataset `data` structures (project ID -\> list of
SINGLE_CELL, SPATIAL, and includes_bulk), taking the union of sample IDs
within each modality. The `includes_bulk` value of existing projects is
preserved; newly added projects use the supplied `include_bulk` value.

## Usage

``` r
merge_dataset_data(existing, additions, include_bulk = FALSE)
```

## Arguments

- existing:

  the current dataset `data` list

- additions:

  a dataset `data` list to merge in (e.g. from
  [`build_dataset_data()`](https://alexslemonade.github.io/ScPCAr/reference/build_dataset_data.md))

- include_bulk:

  logical value to assign to projects that are new to the dataset

## Value

the merged dataset `data` list
