#' Get data frame of all ScPCA projects
#'
#' @param simplify A logical indicating whether to simplify the resulting data frame by removing list columns. Default is TRUE.
#'
#' @returns a data frame of project information from the ScPCA API.
#'
#' @import httr2
#'
#' @export
scpca_projects <- function(simplify = TRUE) {
  responses <- scpca_request("projects") |>
    req_perform_iterative(iterate_scpca)

  df <- responses |>
    resps_data(\(resp) {
      resp_body_json(resp, simplifyVector = TRUE)$results |>
        as.data.frame()
    })

  if (simplify) {
    df <- df |> dplyr::select(where(\(col) !is.list(col)))
  }

  df
}
