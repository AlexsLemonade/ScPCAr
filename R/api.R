#' Base request object for ScPCA API
#'
#' @param resource API resource to query, e.g. "projects", default is "" (base URL)
#' @param body optional named list to include as JSON body in the request
#' @param auth_token optional API authentication token
#' @param ... additional query parameters to include in the request
#'
#' @keywords internal
#'
#' @import httr2
#'
#' @returns a httr2 request object
scpca_request <- function(resource = "", body = list(), auth_token = "", ...) {
  stopifnot(
    "body must be a named list" = is.list(body) && (length(body) == 0 || !is.null(names(body)))
  )
  api_base <- "https://api.scpca.alexslemonade.org/v1" # nolint
  user_agent <- "ScPCA R API Client" # nolint

  req <- httr2::request(api_base) |>
    req_user_agent(user_agent) |>
    req_url_path_append(resource) |>
    req_throttle(capacity = 60) |> # No more than 60 calls per minute
    req_url_query(...)

  if (length(body) > 0) {
    req <- req |> req_body_json(body)
  }

  if (nchar(auth_token) > 0) {
    req <- req |> req_headers_redacted(`api-key` = auth_token)
  }

  req
}

#' Helper function for iterating through paginated ScPCA API results
#'
#' @param resp httr2 response object
#' @param req httr2 request object
#'
#' @returns updated httr2 request object for the next page, or NULL if there are no more pages
#'
#' @keywords internal
#'
#' @import httr2
iterate_scpca <- function(resp, req) {
  body <- resp_body_json(resp)
  url <- body[["next"]]
  if (is.null(url)) {
    return(NULL)
  }
  # calculate total pages and signal to req_perform_iterative
  pages <- ceiling(body[["count"]] / length(body[["results"]]))
  signal_total_pages(pages)
  req |> req_url(url)
}


#' Check if the ScPCA API is reachable
#'
#' This function performs a simple GET request to the ScPCA API to verify that it is reachable.
#'
#' @returns TRUE if the API is reachable, otherwise an error is raised.
#' @import httr2
#'
#' @keywords internal
#'
check_api <- function() {
  status <- tryCatch(
    {
      scpca_request() |>
        req_perform() |>
        resp_status()
    },
    httr2_http_404 = \(cnd) NULL
  )
  if (is.null(response) || status != 200) {
    stop(
      "The API may be down or unreachable. Please check your internet connection or try again later."
    )
  }
  TRUE
}
