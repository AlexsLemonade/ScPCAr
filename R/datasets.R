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


#' Send a PUT request to update a dataset
#'
#' Internal helper that issues a PUT request to `datasets/{dataset_id}` with the
#' supplied body. Datasets are locked once processing has started; the API returns
#' a 409 in that case, which is surfaced here as an informative error.
#'
#' @param dataset_id the dataset UUID string
#' @param body a named list to send as the JSON body of the PUT request
#' @param auth_token an authorization token obtained from [get_auth()]
#'
#' @keywords internal
#'
#' @import httr2
#'
#' @returns the API response as a list
update_dataset <- function(dataset_id, body, auth_token) {
  tryCatch(
    {
      scpca_request(
        resource = paste0("datasets/", dataset_id),
        body = body,
        auth_token = auth_token,
        method = "PUT"
      ) |>
        req_perform() |>
        resp_body_json()
    },
    httr2_http_409 = \(cnd) {
      stop(
        glue::glue(
          "Cannot modify dataset `{dataset_id}`:",
          " it is already processing or has completed.",
          " Datasets are locked once processing has started."
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
#' @param format the desired file format: "sce" (SingleCellExperiment, default) or
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
#'   samples = c("SCPCS000001", "SCPCS000002")
#' )
#' ds$id
#' }
create_dataset <- function(
  auth_token,
  format = "sce",
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


#' Replace the contents of an existing custom dataset
#'
#' Replaces the samples and/or projects in an existing dataset with a new
#' selection, by sending a PUT request with a freshly built `data` field. This
#' is a wholesale replacement: the resulting dataset contains exactly the samples
#' and projects supplied here. To incrementally add or remove samples while
#' keeping the rest, use [add_dataset_samples()] or [remove_dataset_samples()].
#'
#' A dataset that has already been started cannot be updated.
#'
#' @param dataset the dataset UUID string, or a list with an `$id` element.
#' @param auth_token an authorization token obtained from [get_auth()].
#' @param samples optional character vector of ScPCA sample IDs (e.g. "SCPCS000001").
#' @param projects optional character vector of ScPCA project IDs (e.g. "SCPCP000001");
#'   all samples from each project are included.
#' @param include_bulk logical; whether to include bulk RNA-seq files. Default is FALSE.
#'
#' @returns the updated dataset detail as a list (invisibly)
#'
#' @import httr2
#' @export
#'
#' @examples
#' \dontrun{
#' replace_dataset_data(ds, auth_token = token, samples = c("SCPCS000001", "SCPCS000002"))
#' }
replace_dataset_data <- function(
  dataset,
  auth_token,
  samples = NULL,
  projects = NULL,
  include_bulk = FALSE
) {
  stopifnot(
    "At least one of 'samples' or 'projects' must be provided" = !is.null(samples) ||
      !is.null(projects),
    "include_bulk must be a logical value" = is.logical(include_bulk) && length(include_bulk) == 1
  )
  dataset_id <- resolve_dataset_id(dataset)

  data <- build_dataset_data(samples = samples, projects = projects, include_bulk = include_bulk)

  response <- update_dataset(dataset_id, list(data = data), auth_token = auth_token)
  invisible(response)
}


#' Set the notification email for a custom dataset
#'
#' Updates the email address the ScPCA Portal will use to notify you when the
#' dataset is ready for download, by sending a PUT request with a new `email`
#' value. The dataset's samples, projects, and format are left unchanged.
#'
#' A dataset that has already been started cannot be modified.
#'
#' @param dataset the dataset UUID string, or a list with an `$id` element.
#' @param auth_token an authorization token obtained from [get_auth()].
#' @param email the email address to use for the download notification.
#'
#' @returns the updated dataset detail as a list (invisibly)
#'
#' @import httr2
#' @export
#'
#' @examples
#' \dontrun{
#' set_dataset_email(ds, auth_token = token, email = "user@example.com")
#' }
set_dataset_email <- function(dataset, auth_token, email) {
  stopifnot(
    "email must be a single character string" = is.character(email) &&
      length(email) == 1 &&
      nchar(email) > 0
  )
  dataset_id <- resolve_dataset_id(dataset)

  response <- update_dataset(dataset_id, list(email = email), auth_token = auth_token)
  invisible(response)
}


#' Merge additional dataset data into an existing dataset data structure
#'
#' Combines two dataset `data` structures (project ID -> list of SINGLE_CELL,
#' SPATIAL, and includes_bulk), taking the union of sample IDs within each
#' modality. The `includes_bulk` value of existing projects is preserved; newly
#' added projects use the supplied `include_bulk` value.
#'
#' @param existing the current dataset `data` list
#' @param additions a dataset `data` list to merge in (e.g. from [build_dataset_data()])
#' @param include_bulk logical value to assign to projects that are new to the dataset
#'
#' @keywords internal
#'
#' @returns the merged dataset `data` list
merge_dataset_data <- function(existing, additions, include_bulk = FALSE) {
  for (project_id in names(additions)) {
    addition <- additions[[project_id]]
    current <- existing[[project_id]]
    if (is.null(current)) {
      existing[[project_id]] <- addition
      existing[[project_id]]$includes_bulk <- include_bulk
      next
    }
    # Safety check for datasets created outside this package (e.g. directly via the API)
    # where SINGLE_CELL may be the sentinel string "MERGED" rather than a list of IDs.
    if (identical(current$SINGLE_CELL, "MERGED")) {
      stop(
        glue::glue(
          "Project {project_id} uses merged single-cell data (SINGLE_CELL = \"MERGED\")",
          " and cannot be modified by sample. Use replace_dataset_data() to replace its contents."
        ),
        call. = FALSE
      )
    }
    existing[[project_id]]$SINGLE_CELL <- unique(c(current$SINGLE_CELL, addition$SINGLE_CELL))
    existing[[project_id]]$SPATIAL <- unique(c(current$SPATIAL, addition$SPATIAL))
  }
  existing
}


#' Remove samples and/or projects from a dataset data structure
#'
#' Drops any project listed in `projects` entirely, and removes any IDs in
#' `samples` from every project's SINGLE_CELL and SPATIAL lists. A project is
#' removed once both of its modality lists are empty.
#'
#' @param existing the current dataset `data` list
#' @param samples optional character vector of sample IDs to remove
#' @param projects optional character vector of project IDs to remove
#'
#' @keywords internal
#'
#' @returns the reduced dataset `data` list
remove_from_dataset_data <- function(existing, samples = NULL, projects = NULL) {
  # drop whole projects
  if (!is.null(projects)) {
    existing <- existing[setdiff(names(existing), projects)]
  }

  # remove individual samples from each remaining project
  if (!is.null(samples)) {
    for (project_id in names(existing)) {
      current <- existing[[project_id]]
      # Safety check for datasets created outside this package (see merge_dataset_data)
      if (identical(current$SINGLE_CELL, "MERGED")) {
        stop(
          glue::glue(
            "Project {project_id} uses merged single-cell data (SINGLE_CELL = \"MERGED\")",
            " and cannot be modified by sample. Use replace_dataset_data() to replace its contents."
          ),
          call. = FALSE
        )
      }
      # setdiff() requires vectors; convert from list and back
      current$SINGLE_CELL <- as.list(setdiff(as.character(current$SINGLE_CELL), samples))
      current$SPATIAL <- as.list(setdiff(as.character(current$SPATIAL), samples))
      existing[[project_id]] <- current
    }
  }

  # drop any project that no longer has any samples
  keep <- purrr::map_lgl(existing, \(p) length(p$SINGLE_CELL) > 0 || length(p$SPATIAL) > 0)
  existing[keep]
}


#' Add or remove samples and projects in a custom dataset
#'
#' `add_dataset_samples()` adds the given samples and/or all samples from the
#' given projects to an existing dataset, keeping the samples already present.
#' `remove_dataset_samples()` removes the given samples and/or projects, keeping
#' the remaining samples; any project left with no samples is dropped from the
#' dataset.
#'
#' In both cases the current dataset is fetched, its `data` is modified locally
#' (taking the union per project and modality when adding, or dropping the
#' specified IDs when removing), and the updated selection is sent back via a PUT
#' request. To replace the dataset contents wholesale instead, use
#' [replace_dataset_data()].
#'
#' A dataset that has already been started cannot be modified. Projects whose
#' single-cell data is merged (SINGLE_CELL = "MERGED") cannot be modified by
#' sample (though they may be removed wholesale via `projects`); use
#' [replace_dataset_data()] instead.
#'
#' @param dataset the dataset UUID string, or a list with an `$id` element.
#' @param auth_token an authorization token obtained from [get_auth()].
#' @param samples optional character vector of ScPCA sample IDs to add or remove.
#' @param projects optional character vector of ScPCA project IDs to add or
#'   remove; all samples from each project are included.
#' @param include_bulk logical; for `add_dataset_samples()`, the `includes_bulk`
#'   value to use for projects that are newly added to the dataset. Existing
#'   projects keep their current value. Default is FALSE.
#'
#' @returns the updated dataset detail as a list (invisibly)
#'
#' @import httr2
#' @rdname modify_dataset_samples
#' @export
#'
#' @examples
#' \dontrun{
#' add_dataset_samples(ds, auth_token = token, samples = "SCPCS000003")
#' add_dataset_samples(ds, auth_token = token, projects = "SCPCP000002")
#'
#' remove_dataset_samples(ds, auth_token = token, samples = "SCPCS000003")
#' remove_dataset_samples(ds, auth_token = token, projects = "SCPCP000002")
#' }
add_dataset_samples <- function(
  dataset,
  auth_token,
  samples = NULL,
  projects = NULL,
  include_bulk = FALSE
) {
  stopifnot(
    "At least one of 'samples' or 'projects' must be provided" = !is.null(samples) ||
      !is.null(projects),
    "include_bulk must be a logical value" = is.logical(include_bulk) && length(include_bulk) == 1
  )
  dataset_id <- resolve_dataset_id(dataset)

  current <- get_dataset_info(dataset_id, auth_token = auth_token)
  additions <- build_dataset_data(
    samples = samples,
    projects = projects,
    include_bulk = include_bulk
  )
  new_data <- merge_dataset_data(current$data, additions, include_bulk = include_bulk)

  response <- update_dataset(dataset_id, list(data = new_data), auth_token = auth_token)
  invisible(response)
}


#' @rdname modify_dataset_samples
#' @export
remove_dataset_samples <- function(dataset, auth_token, samples = NULL, projects = NULL) {
  stopifnot(
    "At least one of 'samples' or 'projects' must be provided" = !is.null(samples) ||
      !is.null(projects)
  )
  dataset_id <- resolve_dataset_id(dataset)

  current <- get_dataset_info(dataset_id, auth_token = auth_token)
  new_data <- remove_from_dataset_data(current$data, samples = samples, projects = projects)

  response <- update_dataset(dataset_id, list(data = new_data), auth_token = auth_token)
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
