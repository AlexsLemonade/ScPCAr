#' Build the data object for the ScPCA datasets API from sample and project IDs
#'
#' @param samples optional character vector of sample IDs (SCPCS format)
#' @param projects optional character vector of project IDs (SCPCP format)
#' @param include_bulk logical; whether to include bulk RNA-seq files per project
#'
#' @keywords internal
#'
#' @returns a nested list suitable for the `data` field of the datasets API
build_dataset_data <- function(samples = NULL, projects = NULL, include_bulk = FALSE) {
  # get sample ids for each project
  project_sample_ids <- character(0)
  if (!is.null(projects)) {
    project_sample_ids <- projects |>
      purrr::map(\(project_id) get_project_samples(project_id)$scpca_sample_id) |>
      purrr::list_c()
  }
  all_samples <- unique(c(project_sample_ids, samples))

  sample_info <- purrr::map(all_samples, \(sample_id) {
    tryCatch(
      get_sample_info(sample_id, simplifyVector = FALSE),
      error = \(e) e
    )
  })
  # report samples with errors
  failed <- purrr::keep(sample_info, inherits, "error")
  if (length(failed) > 0) {
    stop(
      paste(purrr::map_chr(failed, conditionMessage), collapse = "\n"),
      call. = FALSE
    )
  }

  # organize by project and modality for the API
  by_project <- split(sample_info, purrr::map_chr(sample_info, \(s) s$project$scpca_id))

  purrr::map(by_project, \(project_samples) {
    single_cell <- purrr::keep(project_samples, \(s) isTRUE(s$has_single_cell_data)) |>
      purrr::map("scpca_id")
    spatial <- purrr::keep(project_samples, \(s) isTRUE(s$has_spatial_data)) |>
      purrr::map("scpca_id")
    list(SINGLE_CELL = single_cell, SPATIAL = spatial, includes_bulk = include_bulk)
  })
}


#' Resolve a dataset identifier to its ID string
#'
#' Accepts either a dataset UUID string or a list with an `$id` element (such as
#' the return value of [create_dataset()] or [get_dataset_info()]) and returns
#' the ID string.
#'
#' @param dataset a dataset UUID string, or a list with an `$id` element
#'
#' @keywords internal
#'
#' @returns the dataset ID as a length-1 character string
resolve_dataset_id <- function(dataset) {
  if (is.list(dataset)) {
    stopifnot("dataset must be an id string or contain an $id element" = !is.null(dataset$id))
    return(dataset$id)
  }
  stopifnot(
    "dataset must be an id string or contain an $id element" = is.character(dataset) &&
      length(dataset) == 1
  )
  dataset
}


#' Send a PATCH request to update a dataset
#'
#' Internal helper that issues a PATCH request to `datasets/{dataset_id}` with the
#' supplied body. Datasets are locked once processing has started; the API returns
#' a 409 in that case, which is surfaced here as an informative error.
#'
#' @param dataset_id the dataset UUID string
#' @param body a named list to send as the JSON body of the PATCH request
#' @param auth_token an authorization token obtained from [get_auth()]
#'
#' @keywords internal
#'
#' @import httr2
#'
#' @returns the API response as a list
patch_dataset <- function(dataset_id, body, auth_token) {
  tryCatch(
    {
      scpca_request(
        resource = paste0("datasets/", dataset_id),
        body = body,
        auth_token = auth_token,
        method = "PATCH"
      ) |>
        req_perform() |>
        resp_body_json()
    },
    httr2_http_409 = \(cnd) {
      stop(
        glue::glue(
          "Cannot modify dataset `{dataset_id}`:",
          " it is already processing or has completed.",
          " Datasets are locked once they have been started."
        ),
        call. = FALSE
      )
    }
  )
}


#' Create a custom dataset on the ScPCA Portal
#'
#' Creates a new user dataset without starting processing.
#' After creation, use [get_dataset_info()] to inspect the dataset contents and status.
#'
#' @param auth_token an authorization token obtained from [get_auth()]
#' @param format the desired file format: "sce" (SingleCellExperiment) or
#'   "anndata" (AnnData/H5AD). Spatial data is not a valid format option here;
#'   spatial samples are always returned in Space Ranger format.
#' @param samples optional character vector of ScPCA sample IDs (e.g. "SCPCS000001")
#' @param projects optional character vector of ScPCA project IDs (e.g. "SCPCP000001");
#'   all samples from each project are included
#' @param email optional email address for download notification
#' @param include_bulk logical; whether to include bulk RNA-seq files. Default is FALSE.
#'
#' @returns the API response as a list (invisibly), including the dataset `$id`
#'
#' @import httr2
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_auth("user@example.com", agree = TRUE)
#' ds <- create_dataset(
#'   auth_token = token,
#'   format = "sce",
#'   samples = c("SCPCS000001", "SCPCS000002")
#' )
#' ds$id
#' }
create_dataset <- function(
  auth_token,
  format,
  samples = NULL,
  projects = NULL,
  include_bulk = FALSE,
  email = NULL
) {
  stopifnot(
    "At least one of 'samples' or 'projects' must be provided" = !is.null(samples) ||
      !is.null(projects),
    "Authorization token must be provided" = is.character(auth_token) && nchar(auth_token) > 0,
    "include_bulk must be a logical value" = is.logical(include_bulk) && length(include_bulk) == 1
  )

  format_str <- tryCatch(
    normalize_format(format, allow_spatial = FALSE),
    error = \(e) {
      stop(
        conditionMessage(e),
        "\nFor a dataset with spatial samples, use format = 'sce' or 'anndata';",
        " the spatial samples will always be returned in Space Ranger format.",
        call. = FALSE
      )
    }
  )

  data <- build_dataset_data(samples = samples, projects = projects, include_bulk = include_bulk)

  # create the request body
  body <- list(format = format_str, data = data, start = FALSE)
  if (!is.null(email)) {
    body$email <- email
  }

  response <- scpca_request("datasets", auth_token = auth_token, body = body) |>
    req_perform() |>
    resp_body_json()

  message(glue::glue(
    "Dataset {response$id} created.",
    " Use get_dataset_info() to inspect the dataset."
  ))
  invisible(response)
}


#' Get the status and contents of a custom dataset
#'
#' Returns the full dataset detail, including the `$data` field showing which
#' samples are included and processing status fields such as `$is_started`,
#' `$is_processing`, `$is_succeeded`, and `$is_failed`.
#'
#' @param dataset the dataset UUID string, or a list with an `$id` element
#'   such as the return value of [create_dataset()] or [get_dataset_info()].
#' @param auth_token an authorization token obtained from [get_auth()];
#'  must match the token used to create the dataset.
#'
#' @returns the dataset detail as a list
#'
#' @import httr2
#' @export
#'
#' @examples
#' \dontrun{
#' status <- get_dataset_info("your-dataset-uuid", auth_token = token)
#' status$data         # nested list of projects and samples
#' status$is_succeeded # TRUE when the dataset file is ready for download
#'
#' # You can also pass the result of a previous get_dataset_info() call:
#' status <- get_dataset_info(status, auth_token = token)
#' }
get_dataset_info <- function(dataset, auth_token) {
  dataset_id <- resolve_dataset_id(dataset)
  response <- tryCatch(
    {
      scpca_request(
        resource = paste0("datasets/", dataset_id),
        auth_token = auth_token
      ) |>
        req_perform()
    },
    # return NULL if 404 and the API is reachable (check_api will raise error if not)
    httr2_http_404 = \(cnd) if (check_api()) NULL
  )
  if (is.null(response)) {
    stop(glue::glue(
      "Dataset `{dataset_id}` not found.",
      " Please check the dataset ID and your auth_token.",
      " The token must match the one used to create the dataset."
    ))
  }
  resp_body_json(response)
}


#' Get CCDL dataset objects from the ScPCA API
#'
#' @param project_id Optional ScPCA project ID to filter by (e.g. "SCPCP000001")
#' @param modality Optional modality string to filter by (mapped to `ccdl_modality`)
#' @param format Optional format string to filter by (mapped to `ccdl_format`)
#' @param merged Optional logical to filter merged datasets (mapped to `ccdl_is_merged`)
#' @param include_multiplexed Optional logical to filter by whether the dataset
#'   includes multiplexed files (mapped to `includes_files_multiplexed`)
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
  project_id = NULL,
  modality = NULL,
  format = NULL,
  merged = NULL,
  include_multiplexed = NULL,
  metadata_only = FALSE,
  auth_token = ""
) {
  req <- scpca_request("ccdl-datasets", auth_token = auth_token)

  # append query parameters for any non-NULL arguments
  if (!is.null(project_id)) {
    req <- httr2::req_url_query(req, ccdl_project_id = project_id)
  }
  if (!is.null(modality)) {
    req <- httr2::req_url_query(req, ccdl_modality = modality)
  }
  if (!is.null(format)) {
    req <- httr2::req_url_query(req, format = format)
  }
  if (!is.null(merged)) {
    req <- httr2::req_url_query(req, ccdl_is_merged = merged)
  }
  if (!is.null(include_multiplexed)) {
    req <- httr2::req_url_query(req, includes_files_multiplexed = include_multiplexed)
  }
  if (metadata_only) {
    req <- httr2::req_url_query(req, ccdl_name = "ALL_METADATA")
  }

  datasets <- req |>
    req_perform_iterative(iterate_scpca) |> # no httr2 prefix to allow mocking in tests
    purrr::map(\(resp) resp_body_json(resp)$results) |>
    purrr::list_flatten()
  return(datasets)
}


#' Fetch a single CCDL dataset by ID
#'
#' @param id CCDL dataset ID
#' @param auth_token API authentication token
#'
#' @keywords internal
#' @import httr2
#'
#' @returns the dataset detail as a list
get_ccdl_dataset_detail <- function(id, auth_token) {
  scpca_request(
    resource = paste0("ccdl-datasets/", id),
    auth_token = auth_token
  ) |>
    req_perform() |>
    resp_body_json()
}
