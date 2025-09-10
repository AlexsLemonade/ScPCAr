# Accepted format strings (case insensitive) for the `format` argument in download functions
SCE_FORMATS <- c(
  "sce",
  "singlecellexperiment",
  "single-cell-experiment",
  "single_cell_experiment"
)
ANNDATA_FORMATS <- c("anndata", "h5ad")
SPATIAL_FORMATS <- c("spatial", "spaceranger", "space ranger")


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
#' using the `get_auth()` function.
#'
#'
#' @param sample_id The ScPCA sample ID (e.g. "SCPCS000001")
#' @param auth_token An authorization token obtained from `get_auth()`
#' @param destination The path to the directory where the unzipped file directory should be saved.
#'  Default is "scpca_data".
#' @param format The desired file format, either "sce" (SingleCellExperiment),
#'  "anndata" (AnnData/H5AD), or "spatial" (for spatial data in Space Ranger format).
#'  Default is "sce".
#' @param overwrite Whether to overwrite existing directories if they already exist. Default is FALSE.
#' @param quiet Whether to suppress download progress messages. Default is FALSE.
#'
#' @import httr2
#'
#' @returns a vector of file paths for the downloaded files (invisibly)
#'
#' @export
#' @examples
#' \dontrun{
#' # Get a token first
#' auth_token <- get_auth("me@email.net", agree = TRUE)
#' # Then ask for a sample download
#' download_sample("SCPCS000001", auth_token, destination = "scpca_data", format = "sce")
#'
#' # Downloading in AnnData format
#' download_sample("SCPCS000001", auth_token, destination = "scpca_data", format = "anndata")
#' }
download_sample <- function(
  sample_id,
  auth_token,
  destination = "scpca_data",
  format = "sce",
  overwrite = FALSE,
  quiet = FALSE
) {
  stopifnot(
    "Authorization token must be provided" = is.character(auth_token) && nchar(auth_token) > 0,
    "quiet must be a logical value" = is.logical(quiet) && length(quiet) == 1
  )
  # normalize format input to match API values
  format <- tolower(format)
  if (format %in% SCE_FORMATS) {
    format_str <- "SINGLE_CELL_EXPERIMENT"
  } else if (format %in% ANNDATA_FORMATS) {
    format_str <- "ANN_DATA"
  } else if (format %in% SPATIAL_FORMATS) {
    format_str <- "SPATIAL"
  } else {
    stop(
      "Invalid format. Expected format strings are 'sce', 'anndata', or 'spatial'",
      " (with some additional variants accepted)."
    )
  }
  # create destination directory if it doesn't exist
  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
  }

  sample_info <- get_sample_info(sample_id)

  # message if multiplexed
  if (sample_info$has_multiplexed_data) {
    multiplexed_ids <- sample_info$multiplex |>
      paste(collapse = ", ")
    message(glue::glue(
      "Sample {sample_id} is multiplexed with other samples.",
      " Downloading all associated libraries.",
      " Note that directory names will include all multiplexed sample IDs."
    ))
  }

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
    download_and_extract_file(url, destination, overwrite, quiet)
  }) |>
    purrr::list_c()
  invisible(file_paths)
}


#' Download a project's data files from the ScPCA Portal
#'
#' @param project_id The ScPCA project ID (e.g. "SCPCP000001")
#' @param auth_token An authorization token obtained from `get_auth()`
#' @param destination The path to the directory where the unzipped file directory should be saved.
#'  Default is "scpca_data".
#' @param format The desired file format, either "sce" (SingleCellExperiment),
#'  "anndata" (AnnData/H5AD), or "spatial" (for spatial data in Space Ranger format).
#'  Default is "sce".
#' @param merged Whether to download merged data files, if available.
#'  Default is FALSE.
#' @param overwrite Whether to overwrite existing directories if they already exist. Default is FALSE.
#' @param quiet Whether to suppress download progress messages. Default is FALSE.
#'
#' @returns a vector of file paths for the downloaded files (invisibly)
#'
#' @export
#' @examples
#'
#' \dontrun{
#' # Get a token first
#' auth_token <- get_auth("me@email.net", agree = TRUE)
#' # Then ask for a sample download
#' download_project("SCPCS000001", auth_token, destination = "scpca_data", format = "sce")
#'
#' # Downloading merged files in AnnData format
#' download_project(
#'   "SCPCS000001",
#'   auth_token,
#'   destination = "scpca_data",
#'   format = "anndata",
#'   merged = TRUE
#' )
#' }
download_project <- function(
  project_id,
  auth_token,
  destination = "scpca_data",
  format = "sce",
  merged = FALSE,
  overwrite = FALSE,
  quiet = FALSE
) {
  stopifnot(
    "Authorization token must be provided" = is.character(auth_token) && nchar(auth_token) > 0,
    "quiet must be a logical value" = is.logical(quiet) && length(quiet) == 1,
    "merged must be a logical value" = is.logical(merged) && length(merged) == 1
  )
  # normalize format input to match API values
  format <- tolower(format)
  if (format %in% SCE_FORMATS) {
    format_str <- "SINGLE_CELL_EXPERIMENT"
  } else if (format %in% ANNDATA_FORMATS) {
    format_str <- "ANN_DATA"
  } else if (format %in% SPATIAL_FORMATS) {
    format_str <- "SPATIAL"
  } else {
    stop(
      "Invalid format. Expected format strings are 'sce', 'anndata', or 'spatial'",
      " (with some additional variants accepted)."
    )
  }

  if (format_str == "SPATIAL" && merged) {
    stop("Merged spatial files are not available.")
  }

  # create destination directory if it doesn't exist
  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
  }

  project_info <- get_project_info(project_id)

  files_filter <- computed_files_filter(format_str)
  # add filter for whether to get merged files
  files_filter$includes_merged <- merged

  file_id <- get_computed_file_ids(project_info, filters = files_filter)

  if (length(file_id) == 0) {
    stop(glue::glue("No computed files found for project {project_id} in format {format}."))
  }
  if (length(file_id) > 1) {
    stop("Multiple files found for {project_id} in format {format}; something is wrong?")
  }
  # get signed download URL
  download_url <- scpca_request(
    resource = paste0("computed-files/", file_id),
    auth_token = auth_token
  ) |>
    req_perform() |>
    resp_body_json() |>
    purrr::pluck("download_url")

  file_paths <- download_and_extract_file(download_url, destination, overwrite, quiet)
  invisible(file_paths)
}

#' Download and extract a single file from a URL
#'
#' @param url The download URL
#' @param parent_dir The parent directory where files should be extracted
#' @param overwrite Whether to overwrite existing directories
#' @param quiet Whether to suppress progress messages
#'
#' @returns A character vector of extracted file paths
download_and_extract_file <- function(url, parent_dir, overwrite, quiet) {
  download_filename <- parse_download_file(url)
  destination_dir <- file.path(parent_dir, stringr::str_remove(download_filename, "\\.zip$"))

  # exit if directory already exists
  if (dir.exists(destination_dir) && !overwrite) {
    message(glue::glue(
      "Directory {destination_dir} already exists; skipping download.",
      "\nUse 'overwrite = TRUE' to replace existing files."
    ))
    # no files downloaded, return empty vector
    return(c())
  }

  # TODO: Do we want to warn if a directory exists that matches except for the date?

  file_temp <- file.path(tempdir(), download_filename)
  on.exit(unlink(file_temp), add = TRUE)

  if (!quiet) {
    message(glue::glue("Downloading {download_filename} ..."))
  }
  curl::curl_download(url, file_temp, quiet = quiet)

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
parse_download_file <- function(scpca_url) {
  params <- curl::curl_parse_url(scpca_url)$params
  params["response-content-disposition"] |>
    stringr::str_extract("SCPC[^\\s]+\\.zip")
}


#' Get computed file ids from a sample info list, optionally filtered by criteria
#'
#' @param info_list A list object that includes a "computed_files" element,
#' such as returned by get_sample_info() or get_project_info()
#' @param filters A named list of filtering criteria, where names are fields in
#'  the computed_files objects, and values are the desired values to match.
#'  Values can be negated by prefixing with "!". For example, to get all non-spatial
#'  computed files in SingleCellExperiment format, use:
#'  list(format = "SINGLE_CELL_EXPERIMENT", modality = "!SPATIAL").
#'
#' @returns a character vector of computed file ids matching the filtering criteria
#'
#'
get_computed_file_ids <- function(info_list, filters = list()) {
  stopifnot(
    "info_list must contain a computed_files element" = "computed_files" %in% names(info_list),
    "no computed files found in info_list" = length(info_list$computed_files) > 0,
    "all computed_files must have an id element" = all(
      purrr::map_lgl(info_list$computed_files, \(cf) "id" %in% names(cf))
    ),
    "filters must be a named list of filtering criteria" = {
      is.list(filters) && (length(filters) == 0 || !is.null(names(filters)))
    }
  )

  ids <- info_list$computed_files |>
    purrr::keep(\(cf) {
      all(purrr::imap_lgl(filters, \(val, key) {
        (key %in% names(cf) && !is.null(cf[[key]])) &&
          if (is.character(val) && grepl("^!", val)) {
            # negated filter
            cf[[key]] != stringr::str_remove(val, "^!")
          } else {
            # regular filter
            cf[[key]] == val
          }
      }))
    }) |>
    purrr::map_chr(\(x) as.character(x$id))

  ids
}


#' Helper function to create a filtering list for computed files by format
#'
#' @param format_str a string indicating the desired format
#'
#' @returns a list suitable for passing as the `filters` argument to `get_computed_file_ids()`.
computed_files_filter <- function(
  format_str = c("SINGLE_CELL_EXPERIMENT", "ANN_DATA", "SPATIAL")
) {
  format_str <- match.arg(format_str)
  filter_list = if (format_str == "SPATIAL") {
    list(modality = "SPATIAL")
  } else {
    list(format = format_str, modality = "!SPATIAL")
  }

  filter_list
}
