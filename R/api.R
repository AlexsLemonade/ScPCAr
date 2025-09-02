API_BASE <- "https://api.scpca.alexslemonade.org/v1" # nolint
USER_AGENT <- "ScPCA R API Client" # nolint


#' Base request object for ScPCA API
#'
#' @param resource API resource to query, e.g. "projects"
#' @param body optional named list to include as JSON body in the request
#' @param auth_token optional API authentication token
#' @param ... additional query parameters to include in the request
#'
#' @import httr2
#'
#' @returns a httr2 request object (invisibly)
scpca_request <- function(resource, body = list(), auth_token = "", ...) {
  stopifnot(
    "resource must be a non-empty string" = is.character(resource) && nchar(resource) > 0,
    "body must be a named list" = is.list(body) && (length(body) == 0 || !is.null(names(body)))
  )

  call <- httr2::request(API_BASE) |>
    req_user_agent(USER_AGENT) |>
    req_url_path_append(resource) |>
    req_url_query(...)

  if (nchar(auth_token) > 0) {
    call <- call |> req_headers_redacted(`api-key` = auth_token)
  }

  if (length(body) > 0) {
    call <- call |> req_body_json(body)
  }

  invisible(call)
}
