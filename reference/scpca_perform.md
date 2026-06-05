# Perform an ScPCA API request with centralized error handling

A pipe-friendly drop-in for
[`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
that centralizes handling of common HTTP error statuses (403, 404, 409).
Returns the httr2 response object on success so downstream
`resp_body_json()` calls are unaffected.

## Usage

``` r
scpca_perform(
  req,
  not_found_msg = NULL,
  conflict_msg = NULL,
  forbidden_msg = NULL,
  ...
)
```

## Arguments

- req:

  An httr2 request object, typically built with
  [`scpca_request()`](https://alexslemonade.github.io/ScPCAr/reference/scpca_request.md).

- not_found_msg:

  Character string to use when the API returns 404.

- conflict_msg:

  Character string to use when the API returns 409.

- forbidden_msg:

  Character string to use when the API returns 403.

- ...:

  Additional arguments forwarded to
  [`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html).

## Value

An httr2 response object.
