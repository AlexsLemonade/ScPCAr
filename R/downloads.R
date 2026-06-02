#' Warn about a misplaced auth token in the `destination` argument
#'
#' `auth_token` was previously the second positional argument. If a caller
#' passes a token positionally it would silently land in `destination`.
#' Auth tokens are UUIDs, so a UUID-shaped `destination` almost certainly indicates
#' this mistake; warn so the caller can spot it.
#' (the download itself will fail if the token is misplaced).
#'
#' @param destination the `destination` argument to validate
#'
#' @noRd
warn_destination_is_auth <- function(destination) {
  if (is_uuid(destination)) {
    warning(
      "`destination` looks like an authorization token (a UUID), not a directory path.",
      " `auth_token` is no longer the second positional argument: pass your token with `auth_token = ...`",
      " or set the SCPCA_AUTH_TOKEN environment variable (see `get_auth()`).",
      call. = FALSE
    )
  }
  invisible(destination)
}


#' Download a sample's data files from the ScPCA Portal
#'
#' This function downloads the data files for a specified sample from the ScPCA Portal.
#' The downloaded files are saved in a subdirectory of the specified path, named
#' by the base filename of the downloaded zip file, which includes the sample ID, modality,
#' format, and date.
#'
#' For single-cell and single-nuclei data, files can be downloaded in either
#' SingleCellExperiment ("sce") or AnnData ("anndata") format,
#' and all downloads include the unfiltered, filtered, and processed data objects, as well as
#' associated metadata and QC files.
#'
#' Spatial data, if present, can be downloaded in Space Ranger format using the "spatial" format option.
#'
#' The function returns a vector of file paths for the downloaded files (invisibly).
#'
#' Note that downloading data requires an authorization token, which can be obtained
#' using the [get_auth()] function.
#'
#'
#' @param sample_id The ScPCA sample ID (e.g. "SCPCS000001")
#' @param destination The path to the directory where the unzipped file directory should be saved.
#'  Default is "scpca_data".
#' @param format The desired file format, either "sce" (SingleCellExperiment),
#'  "anndata" (AnnData/H5AD), or "spatial" (for spatial data in Space Ranger format).
#'  Default is "sce".
#' @param overwrite Whether to overwrite files in existing directories if they already exist.
#'  Note that files in existing directories that do not have the same name
#'  as one of the downloaded files will not be deleted.
#'  Default is FALSE.
#' @param redownload Whether to re-download if files from the same sample and format already exist.
#'  If FALSE, existing files will be returned.
#'  Default is FALSE.
#' @param quiet Whether to suppress download progress messages. Default is FALSE.
#' @param auth_token An authorization token from [get_auth()]. Defaults to the
#'  `SCPCA_AUTH_TOKEN` environment variable, which [get_auth()] sets automatically.
#'
#' @import httr2
#'
#' @returns a vector of file paths for the downloaded files (invisibly)
#'
#' @export
#' @examples
#' \dontrun{
#' # get_auth() stores the token in SCPCA_AUTH_TOKEN, so downloads pick it up automatically
#' get_auth("your.email@example.com", agree = TRUE)
#' # Then ask for a sample download
#' download_sample("SCPCS000001", destination = "scpca_data", format = "sce")
#'
#' # Downloading in AnnData format
#' download_sample("SCPCS000001", destination = "scpca_data", format = "anndata")
#' }
download_sample <- function(
  sample_id,
  destination = "scpca_data",
  format = "sce",
  overwrite = FALSE,
  redownload = FALSE,
  quiet = FALSE,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
) {
  warn_destination_is_auth(destination)
  auth_token <- resolve_auth_token(auth_token)
  stopifnot(
    "quiet must be a logical value" = is.logical(quiet) && length(quiet) == 1
  )

  format_str <- normalize_format(format)

  # create destination directory if it doesn't exist
  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
  }
  sample_info <- get_sample_info(sample_id, simplifyVector = FALSE)

  file_ids <- get_computed_file_ids(sample_info, filters = computed_files_filter(format_str))

  if (length(file_ids) == 0) {
    stop(glue::glue("No computed files found for sample {sample_id} in format {format}."))
  }

  # build requests for each file
  # most samples will only have one file, but in some multiplexed cases there may be more
  file_requests <- file_ids |>
    purrr::map(\(id) {
      scpca_request(
        resource = paste0("computed-files/", id),
        auth_token = auth_token
      )
    })

  # get signed download URLs
  download_urls <- req_perform_parallel(file_requests) |>
    resps_data(\(resp) resp_body_json(resp)$download_url)

  file_paths <- purrr::map(download_urls, \(url) {
    download_and_extract_file(url, destination, overwrite, redownload, quiet)
  }) |>
    purrr::list_c()

  # message if multiplexed
  if (sample_info$has_multiplexed_data) {
    message(glue::glue(
      "Sample {sample_id} is multiplexed with other samples.",
      " Downloaded all associated libraries.",
      " Note that directory names will include all multiplexed sample IDs."
    ))
  }

  invisible(file_paths)
}


#' Download a project's data files from the ScPCA Portal
#'
#' @param project_id The ScPCA project ID (e.g. "SCPCP000001")
#' @param destination The path to the directory where the unzipped file directory should be saved.
#'  Default is "scpca_data".
#' @param format The desired file format, either "sce" (SingleCellExperiment),
#'  "anndata" (AnnData/H5AD), or "spatial" (for spatial data in Space Ranger format).
#'  Default is "sce".
#' @param merged Download merged data files, if available.
#'  Default is FALSE.
#' @param include_multiplexed Include multiplexed samples, if available.
#'  Default is TRUE for SingleCellExperiment and FALSE for AnnData and spatial samples,
#'  where multiplexed data are not available.
#' @param overwrite Whether to overwrite files in existing directories if they already exist.
#'  Note that files in existing directories that do not have the same name
#'  as one of the downloaded files will not be deleted.
#'  Default is FALSE.
#' @param redownload Whether to re-download if files from the same project and format already exist.
#'  If FALSE, existing files will be returned.
#'  Default is FALSE.
#' @param quiet Whether to suppress download progress messages. Default is FALSE.
#' @param unzip Whether to unzip the downloaded file. Default is TRUE. When FALSE,
#'   the zip file is saved directly to `destination` and its path is returned.
#' @param auth_token An authorization token from [get_auth()]. Defaults to the
#'  `SCPCA_AUTH_TOKEN` environment variable, which [get_auth()] sets automatically.
#'
#' @importFrom stats setNames
#'
#' @returns a vector of file paths for the downloaded files (invisibly)
#'
#' @export
#' @examples
#' \dontrun{
#' # get_auth() stores the token in SCPCA_AUTH_TOKEN, so downloads pick it up automatically
#' get_auth("your.email@example.com", agree = TRUE)
#' # Then ask for a project download
#' download_project("SCPCP000001", destination = "scpca_data", format = "sce")
#'
#' # Downloading merged files in AnnData format
#' download_project(
#'   "SCPCP000001",
#'   destination = "scpca_data",
#'   format = "anndata",
#'   merged = TRUE
#' )
#' }
download_project <- function(
  project_id,
  destination = "scpca_data",
  format = "sce",
  merged = FALSE,
  include_multiplexed = NULL,
  overwrite = FALSE,
  redownload = FALSE,
  quiet = FALSE,
  unzip = TRUE,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
) {
  warn_destination_is_auth(destination)
  auth_token <- resolve_auth_token(auth_token)
  stopifnot(
    "Invalid project_id." = grepl("^SCPCP\\d{6}$", project_id),
    "quiet must be a logical value" = is.logical(quiet) && length(quiet) == 1,
    "merged must be a logical value" = is.logical(merged) && length(merged) == 1,
    "include_multiplexed must be NULL or a logical value" = is.null(include_multiplexed) ||
      (is.logical(include_multiplexed) && length(include_multiplexed) == 1)
  )

  format_str <- normalize_format(format)

  if (format_str == "SPATIAL" && merged) {
    stop("Merged spatial files are not available.")
  }

  # look up project info to validate the project exists and check for multiplexed data
  project_info <- get_project_info(project_id)
  has_multiplexed <- isTRUE(project_info$has_multiplexed_data)

  # warn if multiplexed was explicitly requested but the project has no multiplexed data
  if (isTRUE(include_multiplexed) && !has_multiplexed) {
    warning(glue::glue(
      "Multiplexed data not available for project {project_id}.",
      " Downloading non-multiplexed data instead."
    ))
  }

  # NULL or TRUE when multiplexed data is available → include multiplexed; otherwise exclude
  multiplexed_query <- has_multiplexed && !identical(include_multiplexed, FALSE)

  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
  }

  # Spatial datasets incorrectly report format = "SINGLE_CELL_EXPERIMENT" in the API;
  # the correct identifier is ccdl_modality = "SPATIAL". Filter by modality as a workaround
  # until the API data is corrected. For non-spatial, also pass modality = "SINGLE_CELL" to
  # exclude spatial datasets on projects that have both (e.g. SCPCP000006).
  if (format_str == "SPATIAL") {
    datasets <- get_ccdl_datasets(
      project_id = project_id,
      modality = "SPATIAL",
      merged = merged,
      include_multiplexed = multiplexed_query,
      auth_token = auth_token
    )
  } else {
    datasets <- get_ccdl_datasets(
      project_id = project_id,
      format = format_str,
      modality = "SINGLE_CELL",
      merged = merged,
      include_multiplexed = multiplexed_query,
      auth_token = auth_token
    )
  }

  # each query should return at most one pre-built dataset
  if (length(datasets) > 1) {
    stop(glue::glue(
      "Multiple pre-built datasets found for project {project_id} in format {format}",
      " (merged = {merged}, include_multiplexed = {deparse(include_multiplexed)}).",
      " This is unexpected; please report this as a bug."
    ))
  }

  dataset <- purrr::pluck(datasets, 1)

  # check that the dataset was successfully processed
  if (!is.null(dataset) && !isTRUE(dataset$is_succeeded)) {
    dataset <- NULL
  }

  if (is.null(dataset)) {
    conditions <- character(0)
    if (merged) {
      conditions <- c(conditions, "merged = TRUE")
    }
    if (!is.null(include_multiplexed)) {
      conditions <- c(conditions, glue::glue("include_multiplexed = {include_multiplexed}"))
    }
    conditions_str <- if (length(conditions) > 0) {
      glue::glue(" (with {paste(conditions, collapse = ' and ')})")
    } else {
      ""
    }
    error_msg <- glue::glue(
      "No pre-built dataset found for project {project_id} in format {format}{conditions_str}."
    )
    stop(error_msg)
  }

  detail <- get_ccdl_dataset_detail(dataset$id, auth_token)
  download_url <- setNames(detail$download_url, detail$download_filename)
  file_paths <- download_and_extract_file(
    url = download_url,
    parent_dir = destination,
    overwrite = overwrite,
    redownload = redownload,
    quiet = quiet,
    unzip = unzip
  )
  invisible(file_paths)
}

#' Download and extract a single file from a URL
#'
#' @param url The download URL
#' @param parent_dir The parent directory where files should be extracted
#' @param overwrite Whether to overwrite existing directories
#' @param redownload Whether to re-download if files from the same url already exist
#'  (if FALSE, existing files will be returned)
#' @param quiet Whether to suppress progress messages
#' @param unzip Whether to unzip the downloaded file. Default is TRUE. When FALSE,
#'   the zip file is saved directly to `parent_dir` and its path is returned.
#'
#' @returns A character vector of extracted file paths, or the zip file path when
#'   `unzip = FALSE`.
#'
#' @keywords internal
download_and_extract_file <- function(url, parent_dir, overwrite, redownload, quiet, unzip = TRUE) {
  download_filename <- if (!is.null(names(url))) names(url) else parse_download_file(url)

  if (!unzip) {
    zip_path <- file.path(parent_dir, download_filename)
    if (file.exists(zip_path) && !overwrite) {
      message(glue::glue(
        "File {zip_path} already exists; skipping download.",
        "\nUse 'overwrite = TRUE' to replace the existing file."
      ))
      return(zip_path)
    }
    req <- httr2::request(unname(url))
    if (!quiet) {
      message(glue::glue("Downloading {download_filename}..."))
      req <- httr2::req_progress(req, type = "down")
    }
    req |> req_perform(path = zip_path)
    return(zip_path)
  }

  destination_dir <- file.path(parent_dir, stringr::str_remove(download_filename, "\\.zip$"))

  # exit if directory already exists
  if (dir.exists(destination_dir) && !overwrite) {
    message(glue::glue(
      "Directory {destination_dir} already exists; skipping download.",
      "\nUse 'overwrite = TRUE' to replace existing files."
    ))
    # return contents of the existing directory
    return(list.files(destination_dir, full.names = TRUE, recursive = TRUE))
  }

  # check for existing directory with same base name (without date)
  destination_basename <- stringr::str_remove(download_filename, "_\\d{4}-\\d{2}-\\d{2}\\.zip$")
  existing_dirs <- list.dirs(parent_dir, full.names = TRUE, recursive = FALSE)
  existing_basenames <- basename(existing_dirs) |>
    stringr::str_remove("_\\d{4}-\\d{2}-\\d{2}$")
  existing_dirs <- existing_dirs[which(existing_basenames == destination_basename)]

  # if found and not overwriting or redownloading, return existing files
  if (length(existing_dirs) > 0 && !redownload && !(dir.exists(destination_dir) && overwrite)) {
    # get the latest file (alphabetically, which works for dates in YYYY-MM-DD format)
    return_dir <- max(existing_dirs)
    message(glue::glue(
      "A directory for {destination_basename} already exists.",
      "\nSkipping download and using existing file paths from the latest download: {return_dir}.",
      "\nUse 'redownload = TRUE' to download a new version of these files."
    ))
    # return existing file paths
    return(list.files(return_dir, full.names = TRUE, recursive = TRUE))
  }

  file_temp <- file.path(tempdir(), download_filename)
  on.exit(unlink(file_temp), add = TRUE)

  req <- httr2::request(unname(url))
  if (!quiet) {
    message(glue::glue("Downloading {download_filename}..."))
    req <- httr2::req_progress(req, type = "down")
  }
  req |> req_perform(path = file_temp)

  if (!quiet) {
    message(glue::glue("Unzipping to {destination_dir}..."))
  }
  utils::unzip(file_temp, exdir = destination_dir)
}

#' Get the base filename from a ScPCA portal download URL
#'
#' (this may become obsolete if we get download filenames in the API response)
#'
#' @param scpca_url The ScPCA portal download URL
#'
#' @returns the download filename
#'
#' @keywords internal
parse_download_file <- function(scpca_url) {
  query <- httr2::url_parse(scpca_url)$query
  query[["response-content-disposition"]] |>
    stringr::str_extract("SCPC[^\\s]+\\.zip") |>
    unname()
}


#' Download a custom dataset's files from the ScPCA Portal
#'
#' Downloads and extracts the files for a custom dataset that has finished
#' processing. The dataset must have a status of "succeeded"; use
#' [get_dataset_status()] to check before calling this function, or use
#' [wait_and_download_dataset()] to wait for processing to complete and then
#' download in a single call.
#'
#' The downloaded files are saved in a subdirectory of `destination`, named
#' from the dataset's download filename (which includes the dataset ID, format,
#' and date).
#'
#' @param dataset the dataset UUID string, or a list with an `$id` element,
#'   such as the return value of [create_dataset()].
#' @param destination The path to the directory where the unzipped file directory
#'   should be saved. Default is "scpca_data".
#' @param overwrite Whether to overwrite files in existing directories if they
#'   already exist. Note that files in existing directories that do not have the
#'   same name as one of the downloaded files will not be deleted. Default is FALSE.
#' @param redownload Whether to re-download if files from the same dataset already
#'   exist. If FALSE, existing files will be returned. Default is FALSE.
#' @param quiet Whether to suppress download progress messages. Default is FALSE.
#' @param unzip Whether to unzip the downloaded file. Default is TRUE. When FALSE,
#'   the zip file is saved directly to `destination` and its path is returned.
#' @param auth_token an authorization token from [get_auth()]. Defaults to the
#'   `SCPCA_AUTH_TOKEN` environment variable, which [get_auth()] sets automatically.
#'
#' @importFrom stats setNames
#'
#' @returns a vector of file paths for the downloaded files (invisibly)
#'
#' @import httr2
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a dataset, start processing, then download once complete
#' ds <- create_dataset(samples = c("SCPCS000001", "SCPCS000002"))
#' start_dataset_processing(ds, email = "user@example.com")
#'
#' # Check status then download when ready
#' get_dataset_status(ds)
#' download_dataset(ds, destination = "scpca_data")
#'
#' # Or use wait_and_download_dataset() to do all of this in one call
#' wait_and_download_dataset(ds, start = TRUE, email = "user@example.com")
#' }
download_dataset <- function(
  dataset,
  destination = "scpca_data",
  overwrite = FALSE,
  redownload = FALSE,
  quiet = FALSE,
  unzip = TRUE,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
) {
  warn_destination_is_auth(destination)
  auth_token <- resolve_auth_token(auth_token)
  stopifnot(
    "unzip must be a logical value" = is.logical(unzip) && length(unzip) == 1,
    "overwrite must be a logical value" = is.logical(overwrite) && length(overwrite) == 1,
    "redownload must be a logical value" = is.logical(redownload) && length(redownload) == 1,
    "quiet must be a logical value" = is.logical(quiet) && length(quiet) == 1
  )
  dataset_id <- resolve_dataset_id(dataset)
  detail <- get_dataset_detail(dataset_id, auth_token)

  if (isTRUE(detail$is_expired)) {
    stop(
      glue::glue(
        "ScPCA dataset `{dataset_id}` has expired and is no longer available for download.",
        " Use `wait_and_download_dataset()` to regenerate it."
      ),
      call. = FALSE
    )
  }

  if (!isTRUE(detail$is_succeeded)) {
    status <- if (isTRUE(detail$is_failed)) {
      "failed"
    } else if (isTRUE(detail$is_processing) || isTRUE(detail$is_started)) {
      "processing"
    } else {
      "pending"
    }
    stop(
      glue::glue(
        "ScPCA dataset `{dataset_id}` is not ready for download (status: {status}).",
        " Use `wait_and_download_dataset()` to wait for processing to complete."
      ),
      call. = FALSE
    )
  }

  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
  }

  download_url <- setNames(detail$download_url, detail$download_filename)

  file_paths <- download_and_extract_file(
    url = download_url,
    parent_dir = destination,
    overwrite = overwrite,
    redownload = redownload,
    quiet = quiet,
    unzip = unzip
  )
  invisible(file_paths)
}


#' @rdname download_dataset
#' @export
#'
#' @param email optional email address for the download notification. Only used
#'   when `start = TRUE`. Passed to [start_dataset_processing()].
#' @param poll_interval Number of minutes to wait between status checks.
#'   Default is 0.5 (30 seconds).
#' @param timeout Maximum number of minutes to wait for processing to complete.
#'   Use `Inf` to wait indefinitely. Default is 60 (1 hour).
wait_and_download_dataset <- function(
  dataset,
  destination = "scpca_data",
  email = NULL,
  overwrite = FALSE,
  redownload = FALSE,
  poll_interval = 0.5,
  timeout = 60,
  quiet = FALSE,
  unzip = TRUE,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
) {
  warn_destination_is_auth(destination)
  auth_token <- resolve_auth_token(auth_token)
  stopifnot(
    "poll_interval must be a single non-negative number of minutes" = is.numeric(poll_interval) &&
      length(poll_interval) == 1 &&
      poll_interval >= 0,
    "timeout must be a single positive number or Inf" = is.numeric(timeout) &&
      length(timeout) == 1 &&
      timeout >= 0,
    "quiet must be a logical value" = is.logical(quiet) && length(quiet) == 1
  )
  dataset_id <- resolve_dataset_id(dataset)

  if (get_dataset_status(dataset_id, auth_token = auth_token) %in% c("pending", "expired")) {
    start_dataset_processing(dataset_id, email = email, auth_token = auth_token)
  }

  start_time <- Sys.time()
  status <- get_dataset_status(dataset_id, auth_token = auth_token)

  if (!quiet) {
    cli::cli_progress_bar(
      format = "{cli::pb_spin} Waiting for dataset {dataset_id} [{status}] {cli::pb_elapsed}",
      clear = FALSE
    )
  }

  repeat {
    if (status == "succeeded") {
      break
    }
    if (status == "failed") {
      if (!quiet) {
        cli::cli_progress_done()
      }
      stop(glue::glue("ScPCA dataset `{dataset_id}` processing failed."), call. = FALSE)
    }
    if (status == "expired") {
      if (!quiet) {
        cli::cli_progress_done()
      }
      stop(
        glue::glue(
          "ScPCA dataset `{dataset_id}` unexpectedly expired during processing.",
          " Please report this as a bug."
        ),
        call. = FALSE
      )
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    if (is.finite(timeout) && elapsed >= timeout) {
      if (!quiet) {
        cli::cli_progress_done()
      }
      stop(
        glue::glue(
          "Timed out after {round(elapsed, 1)} minutes waiting for dataset `{dataset_id}`.",
          " Use `timeout = Inf` to wait indefinitely."
        ),
        call. = FALSE
      )
    }

    if (!quiet) {
      # keep the progress spinner updating every half second until the next poll
      next_loop <- Sys.time() + poll_interval * 60
      while (Sys.time() < next_loop) {
        cli::cli_progress_update(force = TRUE)
        Sys.sleep(0.5)
      }
    } else {
      Sys.sleep(poll_interval * 60)
    }

    status <- get_dataset_status(dataset_id, auth_token = auth_token)
    if (!quiet) cli::cli_progress_update(force = TRUE)
  }

  if (!quiet) {
    cli::cli_progress_done()
  }

  download_dataset(
    dataset_id,
    destination = destination,
    unzip = unzip,
    overwrite = overwrite,
    redownload = redownload,
    quiet = quiet,
    auth_token = auth_token
  )
}
