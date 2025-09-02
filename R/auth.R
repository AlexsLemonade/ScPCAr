#' Get an authorization token from the ScPCA API
#'
#' @param email The user's email address
#' @param agree A logical indicating whether the user agrees to the terms of service
#'
#' @returns A string containing the authorization token
#'
#' @import httr2
#'
#' @export
get_token <- function(email, agree = FALSE) {
  stopifnot(
    "You must agree to the terms of service to get a token." = agree,
    "Invalid email address" = grepl("^[^@]+@[^@]+\\.[^@]+$", email)
  )

  req <- scpca_request("tokens", body = list(email = email, is_activated = agree))

  response <- req_perform(req) |>
    resp_body_json(simplifyVector = TRUE)

  response$id
}
