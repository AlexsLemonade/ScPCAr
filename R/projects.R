#' Get data frame of all ScPCA projects
#'
#' @returns a data frame of project information from the ScPCA API.
#'
#' @import httr2
#'
#' @export
scpca_projects <- function() {
  req <- scpca_request("projects") |>
    req_perform()

  result <- resp_body_json(req, simplifyVector = TRUE)
  df <- result$results

  # If there are more results, continue fetching them
  while (!is.null(result[["next"]])) {
    req <- httr2::request(result[["next"]]) |>
      req_user_agent(USER_AGENT) |>
      req_perform()
    result <- httr2::resp_body_json(req, simplifyVector = TRUE)
    df <- rbind(df, result$results)
  }

  df
}
