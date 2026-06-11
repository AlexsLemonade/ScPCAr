# Get the status and contents of a custom dataset

Returns the full dataset detail, including the `$data` field showing
which samples are included and processing status fields such as
`$is_started`, `$is_processing`, `$is_succeeded`, and `$is_failed`.

## Usage

``` r
get_dataset_detail(dataset, auth_token)
```

## Arguments

- dataset:

  the dataset UUID string (such as the value returned by
  [`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md)),
  or a list with an `$id` element (such as the value returned by
  `get_dataset_detail()`).

- auth_token:

  an authorization token obtained from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md);
  must match the token used to create the dataset.

## Value

the dataset detail as a list

## Details

This is an internal helper intended to be wrapped by higher-level
functions; it is also used by the dataset modification functions to
fetch current contents before updating.
