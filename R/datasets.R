#' Get CCDL dataset objects from the ScPCA API
#'
#' @param project_id Optional ScPCA project ID to filter by (e.g. "SCPCP000001")
#' @param modality Optional modality string to filter by (mapped to `ccdl_modality`)
#' @param format Optional format string to filter by (mapped to `ccdl_format`)
#' @param merged Optional logical to filter merged datasets (mapped to `ccdl_is_merged`)
#' @param metadata_only Logical; if TRUE maps to `ccdl_name = "ALL_METADATA"`
#' @param auth_token Optional API authentication token; when non-empty adds `api-key` header
#'
#' No validation is performed on parameter values; invalid values are passed
#' directly to the API and will result in an API error.
#'
#' @keywords internal
#' @import httr2
#'
#' @returns a list of CCDL dataset objects
get_ccdl_datasets <- function(
  project_id    = NULL,
  modality      = NULL,
  format        = NULL,
  merged        = NULL,
  metadata_only = FALSE,
  auth_token    = ""
) {
  req <- scpca_request("ccdl-datasets", auth_token = auth_token)

  if (!is.null(project_id)) {
    req <- httr2::req_url_query(req, ccdl_project_id = project_id)
  }
  if (!is.null(modality)) {
    req <- httr2::req_url_query(req, ccdl_modality = modality)
  }
  if (!is.null(format)) {
    req <- httr2::req_url_query(req, ccdl_format = format)
  }
  if (!is.null(merged)) {
    req <- httr2::req_url_query(req, ccdl_is_merged = merged)
  }
  if (metadata_only) {
    req <- httr2::req_url_query(req, ccdl_name = "ALL_METADATA")
  }

  responses <- req |> req_perform_iterative(iterate_scpca)

  purrr::map(responses, \(resp) resp_body_json(resp)$results) |>
    purrr::list_flatten()
}
