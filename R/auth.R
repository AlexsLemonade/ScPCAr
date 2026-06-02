#' Get an authorization token from the ScPCA API
#'
#' `get_auth()` allows obtaining an authorization token string from the ScPCA API,
#'  after providing an email address and agreeing to the terms of use.
#'
#' To view the terms of use before agreeing to them, use [view_terms()],
#' which opens the terms of use page in a web browser.
#'
#' The token is stored in the `SCPCA_AUTH_TOKEN` environment variable, which the
#' package's authenticated functions (e.g. [download_sample()]) read automatically,
#' so you usually do not need to pass it explicitly. The token is also returned
#' invisibly, in case you wish to capture it.
#'
#' @param email The user's email address
#' @param agree A logical indicating whether the user agrees to the terms of service
#'
#' @returns The authorization token string (invisibly). The token is also stored
#'   in the `SCPCA_AUTH_TOKEN` environment variable.
#'
#' @import httr2
#'
#' @examples
#' \dontrun{
#' # Get a token (make sure to agree to the terms of service)
#' auth_token <- get_auth("your.email@example.com", agree = TRUE)
#' }
#'
#' @export
get_auth <- function(email, agree = FALSE) {
  stopifnot(
    "You must agree to the terms of service to get a token." = agree,
    "Invalid email address." = grepl("^[^@]+@[^@]+\\.[^@]+$", email)
  )

  req <- scpca_request("tokens", body = list(email = email, is_activated = agree))

  response <- withCallingHandlers(
    req_perform(req) |> resp_body_json(simplifyVector = TRUE),
    # if we get a 400 error, it is probably a fake email address
    httr2_http_400 = \(cnd) {
      resp <- last_response() |> resp_body_json(simplifyVector = TRUE)
      if (!is.null(resp[["email"]])) {
        stop(
          "get_auth() failed to get token: ",
          paste(resp[["email"]], collapse = " "),
          call. = FALSE
        )
      }
      stop(
        "get_auth() failed to get token: Please check the email address and try again.",
        call. = FALSE
      )
    }
  )

  token <- response$id
  Sys.setenv(SCPCA_AUTH_TOKEN = token)
  invisible(token)
}


#' Resolve an authorization token, falling back to the environment
#'
#' Returns the supplied `auth_token` if it is a non-empty string. Otherwise it
#' falls back to the `SCPCA_AUTH_TOKEN` environment variable, which is set
#' automatically by [get_auth()]. An informative error is raised if neither
#' source yields a token.
#'
#' @param auth_token an authorization token string. Defaults to the
#'   `SCPCA_AUTH_TOKEN` environment variable.
#'
#' @keywords internal
#'
#' @returns the resolved token as a length-1 character string
resolve_auth_token <- function(auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")) {
  if (length(auth_token) != 1 || is.null(auth_token) || !nzchar(auth_token)) {
    stop(
      "Authorization token must be provided via the `auth_token` argument",
      " or the `SCPCA_AUTH_TOKEN` environment variable",
      " (use `get_auth()` to obtain one and set the environment variable).",
      call. = FALSE
    )
  }
  auth_token
}


#' View Terms of Use
#' @export
#' @rdname get_auth
#' @examples
#' \dontrun{
#' view_terms()
#' }
view_terms <- function() {
  url <- "https://scpca.alexslemonade.org/terms-of-use"
  utils::browseURL(url)
  invisible(url)
}
