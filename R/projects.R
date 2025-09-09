#' Get data frame of all ScPCA projects
#'
#' This retrieves the full list of projects from the ScPCA Portal and returns
#' a data frame of project metadata. By default, list columns are removed to create
#' a simplified data frame, but this can be disabled by setting `simplify = FALSE`.
#' The unsimplified data frame contains nested list columns with additional details
#' about the samples within each project, such as the set of diagnoses associated with
#' each project and the individual sample ids.
#'
#' @param simplify A logical indicating whether to simplify the resulting data frame
#'  by removing list columns. Default is TRUE.
#'
#' @returns a data frame of project information from the ScPCA API.
#'
#' @import httr2
#' @importFrom dplyr .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # a simplified data frame of all projects
#' project_df <- scpca_projects()
#'
#' # a data frame of all projects without simplification
#' project_df_full <- scpca_projects(simplify = FALSE)
#' }
scpca_projects <- function(simplify = TRUE) {
  responses <- scpca_request("projects", limit = 50) |>
    req_perform_iterative(iterate_scpca)

  project_df <- responses |>
    resps_data(\(resp) {
      resp_body_json(resp, simplifyVector = TRUE)$results |>
        as.data.frame()
    })

  if (simplify) {
    project_df <- project_df |>
      dplyr::select(dplyr::where(\(col) !is.list(col)))
  }

  # convert types and relocate columns
  project_df <- project_df |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), \(x) dplyr::na_if(x, "NA")),
      created_at = as.POSIXct(.data$created_at),
      updated_at = as.POSIXct(.data$updated_at)
    ) |>
    dplyr::relocate(scpca_project_id = "scpca_id")

  project_df
}

#' Get a data frame of all samples in a given project
#'
#' This function retrievs a data frame of all biological samples associated with a SCPCA project,
#' including sample-level metadata. By default, list columns are removed to create
#' a simplified data frame, but this can be disabled by setting `simplify = FALSE`.
#' The unsimplified data frame contains nested list columns with additional details,
#' such as the experimental modalities associated with each sample.
#'
#' @param project_id The project ID (e.g. "SCPCP000001")
#' @param simplify A logical indicating whether to simplify the resulting data frame
#'  by removing list columns. Default is TRUE.
#'
#' @returns a data frame of sample information for the specified project from the ScPCA API.
#'
#' @import httr2
#' @importFrom dplyr .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get sample info for a specific project
#' samples_df <- get_project_samples("SCPCP000001")
#'
#' # Get sample info without simplifying
#' samples_df_full <- get_project_samples("SCPCP000001", simplify = FALSE)
#' }
get_project_samples <- function(project_id, simplify = TRUE) {
  stopifnot(
    "Invalid project_id." = grepl("^SCPCP\\d{6}$", project_id)
  )

  response <- tryCatch(
    {
      scpca_request(paste0("projects/", project_id)) |>
        req_perform()
    },
    # return NULL if 404
    httr2_http_404 = \(cnd) NULL
  )
  # if we get a 404, return empty df
  if (is.null(response)) {
    stop(glue::glue("Project `{project_id}` not found."))
  }

  sample_df <- resp_body_json(response, simplifyVector = TRUE)$samples |>
    as.data.frame() |>
    # unnest additional_metadata column
    tidyr::unnest("additional_metadata")

  if (simplify) {
    sample_df <- sample_df |>
      dplyr::select(dplyr::where(\(col) !is.list(col)))
  }

  # reorganize and recast columns
  sample_df <- sample_df |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), \(x) dplyr::na_if(x, "NA")),
      age = as.numeric(.data$age),
      created_at = as.POSIXct(.data$created_at),
      updated_at = as.POSIXct(.data$updated_at)
    ) |>
    dplyr::relocate(scpca_sample_id = "scpca_id", scpca_project_id = "project")

  sample_df
}
