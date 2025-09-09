get_sample_info <- function(sample_id) {
  stopifnot(
    "Invalid sample_id" = grepl("^SCPCS\\d{6}$", sample_id)
  )
  sample_info <- tryCatch(
    {
      scpca_request(paste0("samples/", sample_id)) |>
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
