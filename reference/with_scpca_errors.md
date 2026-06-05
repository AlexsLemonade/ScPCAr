# Centralized error handler for ScPCA API requests

Wraps an expression that performs an httr2 request and converts common
HTTP error statuses into informative messages. All three statuses can be
overridden with a custom message via the corresponding argument.

## Usage

``` r
with_scpca_errors(
  expr,
  not_found_msg = NULL,
  conflict_msg = NULL,
  forbidden_msg = NULL
)
```

## Arguments

- expr:

  An expression that performs an httr2 request, evaluated lazily inside
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html).

- not_found_msg:

  Character string to use when the API returns 404. Defaults to a
  generic "not found" message.

- conflict_msg:

  Character string to use when the API returns 409. Defaults to a
  generic "conflict" message.

- forbidden_msg:

  Character string to use when the API returns 403. Defaults to a
  generic authorization-failure message.

## Value

The value of `expr` if the request succeeds (i.e. an httr2 response
object, or whatever the expression returns on success).
