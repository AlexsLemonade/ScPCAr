# Remove samples and/or projects from a dataset data structure

Drops any project listed in `projects` entirely, and removes any IDs in
`samples` from every project's SINGLE_CELL and SPATIAL lists. A project
is removed once both of its modality lists are empty.

## Usage

``` r
remove_from_dataset_data(existing, samples = NULL, projects = NULL)
```

## Arguments

- existing:

  the current dataset `data` list

- samples:

  optional character vector of sample IDs to remove

- projects:

  optional character vector of project IDs to remove

## Value

the reduced dataset `data` list
