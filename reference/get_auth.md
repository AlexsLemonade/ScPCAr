# Get an authorization token from the ScPCA API

`get_auth()` allows obtaining an authorization token string from the
ScPCA API, after providing an email address and agreeing to the terms of
use.

## Usage

``` r
get_auth(email, agree = FALSE)

view_terms()
```

## Arguments

- email:

  The user's email address

- agree:

  A logical indicating whether the user agrees to the terms of service

## Value

The authorization token string (invisibly). The token is also stored in
the `SCPCA_AUTH_TOKEN` environment variable.

## Details

To view the terms of use before agreeing to them, use `view_terms()`,
which opens the terms of use page in a web browser.

The token is stored in the `SCPCA_AUTH_TOKEN` environment variable,
which the package's authenticated functions (e.g.
[`download_sample()`](https://alexslemonade.github.io/ScPCAr/reference/download_sample.md))
read automatically, so you usually do not need to pass it explicitly.
The token is also returned invisibly, in case you wish to capture it.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a token (make sure to agree to the terms of service)
auth_token <- get_auth("your.email@example.com", agree = TRUE)
} # }

if (FALSE) { # \dontrun{
view_terms()
} # }
```
