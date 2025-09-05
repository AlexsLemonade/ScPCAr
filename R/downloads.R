download_sample <- function(sample_id, auth_token, path = "scpca_data", format = "sce") {
  stopifnot(
    "Invalid sample_id" = grepl("^SCPCS\\d{6}$", sample_id),
    "path must be a valid directory" = dir.exists(path),
    "Authorization token must be provided" = is.character(auth_token) && nchar(auth_token) > 0
  )

  sce_formats <- c(
    "sce",
    "singlecellexperiment",
    "single-cell-experiment",
    "single_cell_experiment"
  )
  anndata_formats <- c("anndata", "h5ad")

  if (tolower(format) %in% sce_formats) {
    format_str <- "SINGLE_CELL_EXPERIMENT"
  } else if (tolower(format) %in% anndata_formats) {
    format_str <- "ANN_DATA"
  } else {
    stop("Invalid format. Supported formats are: 'sce', 'anndata', 'h5ad'.")
  }

  sample_info <- tryCatch(
    {
      scpca_request(paste0("samples/", sample_id)) |>
        req_perform() |>
        resp_body_json()
    },
    # return NULL if 404
    httr2_http_404 = \(cnd) NULL
  )

  if (is.null(sample_info)) {
    stop(glue::glue("Sample `{sample_id}` not found."))
  }

  file_list <- sample_info$computed_files |>
    purrr::keep(\(f) f$format == format_str)
  if (length(file_list) == 0) {
    stop(glue::glue("No files found for sample {sample_id} in format {format}."))
  }

  # build requests for each file
  # there really should only be one file per sample/format, but just in case
  # I might be missing something about spatial data
  file_requests <- file_list |>
    purrr::map_chr(as.character("id")) |>
    purrr::map(\(id) {
      scpca_request(
        resource = paste0("computed-files/", id),
        auth_token = auth_token
      )
    })

  # get signed download URLs
  download_urls <- req_perform_parallel(file_requests) |>
    resps_data(\(resp) resp_body_json(resp)$download_url)

  file_paths <- purrr::map_chr(download_urls, \(url) {
    base_dir <- extract_download_basename(url)
    download_dir <- file.path(path, base_dir)

    file_temp <- tempfile(pattern = "scpca_download", fileext = ".zip")
    on.exit(unlink(file_temp), add = TRUE)

    message(glue::glue("Downloading {base_dir} ..."))
    curl::curl_download(url, file_temp, quiet = FALSE)

    message(glue::glue("Unzipping to {download_dir}..."))
    unzip(file_temp, exdir = download_dir)
  })
  invisible(file_paths)
}

#' Get the base filename from a ScPCA portal download URL
#'
#' (this may become obsolete if we get download filenames in the API response)
#'
#' @param url
#'
#' @returns the base filename without the .zip extension
extract_download_basename <- function(url) {
  params <- curl::curl_parse_url(file_url)$params
  params["response-content-disposition"] |>
    stringr::str_extract("SCPC[^\\s]+(?=\\.zip)")
}
