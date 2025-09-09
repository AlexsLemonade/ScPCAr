get_sample_info <- function(sample_id, auth_token = NULL) {
  stopifnot(
    "Invalid sample_id" = grepl("^SCPCS\\d{6}$", sample_id),
    "Authorization token, if provided, must be a non-empty string." = {
      is.null(auth_token) || (is.character(auth_token) && nchar(auth_token) > 0)
    }
  )
  sample_info <- tryCatch(
    {
      scpca_request(paste0("samples/", sample_id), auth_token = auth_token) |>
        req_perform() |>
        resp_body_json()
    },
    # return NULL if 404 and the API is reachable (check_api will raise error if not)
    httr2_http_404 = \(cnd) if (check_api()) NULL
  )
  if (is.null(sample_info)) {
    stop(glue::glue("Sample `{sample_id}` not found."))
  }
  sample_info
}
