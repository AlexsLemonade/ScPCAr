#' Get an authorization token from the ScPCA API
#'
#' `get_auth()` allows obtaining an authorization token string from the ScPCA API,
#'  after providing an email address and agreeing to the terms of use.
#'
#' To view the terms of use before agreeing to them, use `view_terms()`, which opens the terms of use page in a web browser.
#'
#' @param email The user's email address
#' @param agree A logical indicating whether the user agrees to the terms of service
#'
#' @returns A string containing the authorization token
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

  response <- req_perform(req) |>
    resp_body_json(simplifyVector = TRUE)

  response$id
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
