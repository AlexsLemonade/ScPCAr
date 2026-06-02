# Send a PUT request to update a dataset

Internal helper that issues a PUT request to `datasets/{dataset_id}`
with the supplied body. Datasets are locked once processing has started;
the API returns a 409 in that case, which is surfaced here as an
informative error.

## Usage

``` r
update_dataset(dataset_id, body, auth_token)
```

## Arguments

- dataset_id:

  the dataset UUID string

- body:

  a named list to send as the JSON body of the PUT request

- auth_token:

  an authorization token obtained from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)

## Value

the API response as a list
