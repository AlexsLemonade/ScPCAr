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
  project_sample_ids <- if (!is.null(projects)) {
    purrr::map_chr(projects, \(project_id) get_project_samples(project_id)$scpca_id) |>
      c()
  } else {
    character(0)
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
    stop(paste(purrr::map_chr(failed, conditionMessage), collapse = "\n"), call. = FALSE)
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


#' Create a custom dataset on the ScPCA Portal
#'
#' Creates a new user dataset without starting processing.
#' After creation, use [update_dataset()] to add or remove samples,
#' and [get_dataset_status()] to inspect the dataset contents and status.
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
#' @returns the API response as a list (invisibly), including the dataset `id`
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

  format_str <- normalize_format(format)
  if (format_str == "SPATIAL") {
    stop(
      "'spatial' is not a valid format for datasets. Spatial data is always returned in Space Ranger format."
    )
  }

  data <- build_dataset_data(samples = samples, projects = projects, include_bulk = include_bulk)

  body <- list(format = format_str, data = data, start = FALSE)
  if (!is.null(email)) {
    body$email <- email
  }

  response <- scpca_request("datasets", auth_token = auth_token, body = body) |>
    req_perform() |>
    resp_body_json()

  message(glue::glue(
    "Dataset {response$id} created.",
    " Use update_dataset() to add or remove samples,",
    " or get_dataset_status() to inspect the dataset."
  ))
  invisible(response)
}


#' Get the status and contents of a custom dataset
#'
#' Returns the full dataset detail, including the `$data` field showing which
#' samples are included and processing status fields such as `$is_started`,
#' `$is_processing`, `$is_succeeded`, and `$is_failed`.
#'
#' @param dataset_id the dataset UUID returned by [create_dataset()]
#' @param auth_token an authorization token obtained from [get_auth()]
#'
#' @returns the dataset detail as a list
#'
#' @import httr2
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_auth("user@example.com", agree = TRUE)
#' status <- get_dataset_status("your-dataset-uuid", token)
#' status$data         # nested list of projects and samples
#' status$is_succeeded # TRUE when the dataset file is ready for download
#' }
get_dataset_status <- function(dataset_id, auth_token) {
  scpca_request(
    resource = paste0("datasets/", dataset_id),
    auth_token = auth_token
  ) |>
    req_perform() |>
    resp_body_json()
}


#' Update the samples in a user dataset
#'
#' Adds or removes samples and projects from an existing dataset using a
#' partial update (PATCH). At least one of the add/remove parameters must
#' be provided.
#'
#' @param dataset_id the dataset UUID returned by [create_dataset()]
#' @param auth_token an authorization token obtained from [get_auth()]
#' @param add_samples optional character vector of sample IDs to add
#' @param add_projects optional character vector of project IDs whose samples
#'   should be added
#' @param remove_samples optional character vector of sample IDs to remove
#' @param remove_projects optional character vector of project IDs to remove
#'   (removes all samples from those projects)
#' @param include_bulk optional logical; if provided, updates `includes_bulk`
#'   for all projects in the dataset
#'
#' @returns the updated dataset detail as a list (invisibly)
#'
#' @import httr2
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_auth("user@example.com", agree = TRUE)
#' update_dataset(
#'   "your-dataset-uuid",
#'   auth_token = token,
#'   add_samples = "SCPCS000003"
#' )
#' update_dataset(
#'   "your-dataset-uuid",
#'   auth_token = token,
#'   remove_samples = c("SCPCS000001", "SCPCS000002")
#' )
#' }
update_dataset <- function(
  dataset_id,
  auth_token,
  add_samples = NULL,
  add_projects = NULL,
  remove_samples = NULL,
  remove_projects = NULL,
  include_bulk = NULL
) {
  has_additions <- !is.null(add_samples) || !is.null(add_projects)
  has_removals <- !is.null(remove_samples) || !is.null(remove_projects)

  stopifnot(
    "At least one of add_samples, add_projects, remove_samples, remove_projects, or include_bulk must be provided" = has_additions ||
      has_removals ||
      !is.null(include_bulk),
    "Authorization token must be provided" = is.character(auth_token) && nchar(auth_token) > 0
  )

  current <- get_dataset_status(dataset_id, auth_token)
  data <- current$data

  # Apply additions: merge new samples into existing data
  if (has_additions) {
    new_data <- build_dataset_data(samples = add_samples, projects = add_projects)

    for (project_id in names(new_data)) {
      if (is.null(data[[project_id]])) {
        data[[project_id]] <- new_data[[project_id]]
      } else {
        existing_sc <- unlist(data[[project_id]]$SINGLE_CELL)
        existing_sp <- unlist(data[[project_id]]$SPATIAL)
        new_sc <- unlist(new_data[[project_id]]$SINGLE_CELL)
        new_sp <- unlist(new_data[[project_id]]$SPATIAL)
        data[[project_id]]$SINGLE_CELL <- as.list(union(existing_sc, new_sc))
        data[[project_id]]$SPATIAL <- as.list(union(existing_sp, new_sp))
      }
    }
  }

  # Remove entire projects
  if (!is.null(remove_projects)) {
    data[remove_projects] <- NULL
  }

  # Remove individual samples from all projects; drop projects with no remaining samples
  if (!is.null(remove_samples)) {
    for (project_id in names(data)) {
      existing_sc <- unlist(data[[project_id]]$SINGLE_CELL)
      existing_sp <- unlist(data[[project_id]]$SPATIAL)
      data[[project_id]]$SINGLE_CELL <- as.list(setdiff(existing_sc, remove_samples))
      data[[project_id]]$SPATIAL <- as.list(setdiff(existing_sp, remove_samples))
    }
    data <- purrr::keep(data, \(proj) {
      length(proj$SINGLE_CELL) > 0 || length(proj$SPATIAL) > 0
    })
  }

  # Update includes_bulk for all remaining projects
  if (!is.null(include_bulk)) {
    for (project_id in names(data)) {
      data[[project_id]]$includes_bulk <- include_bulk
    }
  }

  response <- scpca_request(
    resource = paste0("datasets/", dataset_id),
    auth_token = auth_token,
    body = list(data = data)
  ) |>
    httr2::req_method("PATCH") |>
    req_perform() |>
    resp_body_json()

  invisible(response)
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
