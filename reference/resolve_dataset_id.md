# Resolve a dataset identifier to its ID string

Accepts either a dataset UUID string or a list with an `$id` element
(such as the return value of
[`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md)
or
[`get_dataset_detail()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_detail.md))
and returns the ID string, after checking that it is a valid UUID.

## Usage

``` r
resolve_dataset_id(dataset)
```

## Arguments

- dataset:

  a dataset UUID string, or a list with an `$id` element

## Value

the dataset ID as a length-1 character string
