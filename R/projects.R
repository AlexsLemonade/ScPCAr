#' Get data frame of all ScPCA projects
#'
#' @returns a data frame of project information from the ScPCA API.
#'
#' @import httr2
#'
#' @export
scpca_projects <- function() {
  responses <- scpca_request("projects") |>
    req_perform_iterative(iterate_scpca)

  df <- responses |>
    resps_data(\(resp) {
      resp_body_json(resp, simplifyVector = TRUE)$results |>
        as.data.frame()
    })

  df
}
