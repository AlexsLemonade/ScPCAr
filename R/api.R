API_BASE <- "https://api.scpca.alexslemonade.org/v1" # nolint
USER_AGENT <- "ScPCA R API Client" # nolint
#' Base request object for ScPCA API
#'
#' @param resource API resource to query, e.g. "projects"
#' @param limit number of records to return (default 20)
#' @param offset number of records to skip (default 0)
#'
#' @import httr2
#'
#' @returns a httr2 request object
scpca_request <- function(resource, limit = 20, offset = 0) {
  httr2::request(API_BASE) |>
    httr2::req_user_agent(USER_AGENT) |>
    httr2::req_url_path_append(resource) |>
    httr2::req_url_query(
      limit = limit,
      offset = offset
    )
}


#' Get data frame of all ScPCA projects
#'
#' @returns a data frame of project information from the ScPCA API.
#'
#' @export
scpca_projects <- function() {
  req <- scpca_request("projects") |>
    httr2::req_perform()

  result <- httr2::resp_body_json(req, simplifyVector = TRUE)
  df <- result$results

  # If there are more results, continue fetching them
  while (!is.null(result[["next"]])) {
    req <- httr2::request(result[["next"]]) |>
      httr2::req_user_agent(USER_AGENT) |>
      httr2::req_perform()
    result <- httr2::resp_body_json(req, simplifyVector = TRUE)
    df <- rbind(df, result$results)
  }

  df
}
