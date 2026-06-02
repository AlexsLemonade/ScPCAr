# Resolve an authorization token, falling back to the environment

Returns the supplied `auth_token` if it is a non-empty string. Otherwise
it falls back to the `SCPCA_AUTH_TOKEN` environment variable, which is
set automatically by
[`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
An informative error is raised if neither source yields a token.

## Usage

``` r
resolve_auth_token(auth_token = Sys.getenv("SCPCA_AUTH_TOKEN"))
```

## Arguments

- auth_token:

  an authorization token string. Defaults to the `SCPCA_AUTH_TOKEN`
  environment variable.

## Value

the resolved token as a length-1 character string
