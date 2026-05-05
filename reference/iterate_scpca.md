# Helper function for iterating through paginated ScPCA API results

Helper function for iterating through paginated ScPCA API results

## Usage

``` r
iterate_scpca(resp, req)
```

## Arguments

- resp:

  httr2 response object

- req:

  httr2 request object

## Value

updated httr2 request object for the next page, or NULL if there are no
more pages
