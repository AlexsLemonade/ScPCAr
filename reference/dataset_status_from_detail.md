# Map dataset detail status flags to a status string

Map dataset detail status flags to a status string

## Usage

``` r
dataset_status_from_detail(detail)
```

## Arguments

- detail:

  the dataset detail list returned by
  [`get_dataset_detail()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_detail.md)

## Value

a single character string: one of "pending", "processing", "succeeded",
"failed", or "expired"
