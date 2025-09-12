#' Get computed file ids from a sample info list, optionally filtered by criteria
#'
#' @param info_list A list object that includes a "computed_files" element,
#'  such as returned by get_sample_info() or get_project_info()
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

  filtered_files <- info_list$computed_files |>
    purrr::keep(\(cf) {
      # check every key/value in the filter list
      all(purrr::imap_lgl(filters, \(val, key) {
        (key %in% names(cf) && !is.null(cf[[key]])) && # key is present
          if (is.character(val) && grepl("^!", val)) {
            # key does not have negated value
            cf[[key]] != stringr::str_remove(val, "^!")
          } else {
            # key has value
            cf[[key]] == val
          }
      }))
    })

  ids <- filtered_files |>
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
  filter_list <- if (format_str == "SPATIAL") {
    list(modality = "SPATIAL")
  } else {
    list(format = format_str, modality = "!SPATIAL")
  }

  filter_list
}
