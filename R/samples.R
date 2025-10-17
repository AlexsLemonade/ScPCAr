#' Get sample metadata by sample ID
#'
#' @param sample_id The ScPCA sample ID (e.g. "SCPCS000001")
#' @param simplifyVector Simplify the returned list structure,
#'  creating vectors and data frames instead of lists when possible.
#'  Default is TRUE.
#'
#' @returns A nested list of sample metadata from the ScPCA API.
#'
#' @import httr2
#'
#' @export
#' @examples
#' \dontrun{
#' # Get metadata for a specific sample
#' sample_info <- get_sample_info("SCPCS000001")
#' }
get_sample_info <- function(sample_id, simplifyVector = TRUE) {
  stopifnot(
    "Invalid sample_id" = grepl("^SCPCS\\d{6}$", sample_id)
  )
  sample_info <- tryCatch(
    {
      scpca_request(paste0("samples/", sample_id)) |>
        req_perform() |>
        resp_body_json(simplifyVector = simplifyVector)
    },
    # return NULL if 404 and the API is reachable (check_api will raise error if not)
    httr2_http_404 = \(cnd) if (check_api()) NULL
  )
  if (is.null(sample_info)) {
    stop(glue::glue("Sample `{sample_id}` not found."))
  }
  sample_info
}
