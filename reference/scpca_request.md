# Base request object for ScPCA API

Base request object for ScPCA API

## Usage

``` r
scpca_request(
  resource = "",
  body = list(),
  auth_token = "",
  method = NULL,
  ...
)
```

## Arguments

- resource:

  API resource to query, e.g. "projects", default is "" (base URL)

- body:

  optional named list to include as JSON body in the request

- auth_token:

  optional API authentication token

- method:

  optional HTTP method to use (e.g. "PATCH"). When NULL (the default),
  httr2 infers the method: POST when a body is present, otherwise GET.

- ...:

  additional query parameters to include in the request

## Value

a httr2 request object
