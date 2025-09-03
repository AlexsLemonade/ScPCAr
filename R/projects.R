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

get_project_samples <- function(project_id, simplify = TRUE) {
  stopifnot(
    "Invalid project_id." = grepl("^SCPCP\\d{6}$", project_id)
  )

  response <- scpca_request(paste0("projects/", project_id)) |>
    req_perform()

  df <- resp_body_json(response, simplifyVector = TRUE)$samples |>
    as.data.frame()

  if (simplify) {
    # todo: pull out additional metadata
    df <- df |> dplyr::select(where(\(col) !is.list(col)))
  }

  # reorder and return
  df |>
    dplyr::relocate(sample_id = scpca_id, project_id = project)
}
