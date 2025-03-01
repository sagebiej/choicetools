#' Create sets from given data
#'
#' This function creates sets based on given attributes and choice columns.
#' It groups data by unique rows, calculates counts, and pivots the data for a
#' comprehensive overview.
#'
#' @param .data A data frame or tibble containing the data.
#' @param choice A string specifying the choice column.
#' @param attributes A selection condition for attribute columns (tidyselect compatible).
#' @param uniquerow A string specifying the unique row identifier column.
#' @param prefix A string specifying the prefix for renaming (default is "a").
#'
#' @return A list of tibbles representing the sets.
#'
#' @export
#'
#' @examples \dontrun{
#' # Given a package dataset 'sample_data':
#' createSets(sample_data, choice = "choice_col", attributes = starts_with("attr"), uniquerow = "id") }







createSets <- function(.data, choice, attributes , uniquerow, prefix="a") {
  # require("dplyr")
  # require("tidyr")
  # require("purrr")

  if (!is.data.frame(.data)) {
    stop("The input data (.data) must be a data frame or tibble.")
  }
  if (!all(c(choice, uniquerow) %in% names(.data))) {
    stop("Both choice and uniquerow columns must exist in the input data.")
  }
  attribute_cols <- dplyr::select(.data, {{ attributes }}) %>% names()
  if (length(setdiff(attribute_cols, names(.data))) > 0) {
    stop("Some columns specified in attributes do not exist in the input data.")
  }

  if (any(is.na(.data[c(choice, uniquerow, attribute_cols)]))) {
    stop("The columns choice, uniquerow, and attributes should not have missing values.")
  }



  sets <- .data %>%
    dplyr::select({{ attributes }}, {{ choice }}, {{ uniquerow }} ) %>%
    dplyr::group_by(!!rlang::sym(uniquerow), !!rlang::sym(choice)) %>%
    dplyr::add_count() %>% dplyr::ungroup() %>%
    dplyr::group_by(!!rlang::sym(uniquerow)) %>%
    dplyr::distinct(n, .keep_all=TRUE) %>%
    dplyr::mutate(perc = round((n / sum(n) * 100))) %>%
    dplyr::arrange({{ uniquerow }}, {{ choice }}) %>%
    dplyr::group_split() %>%
    purrr::set_names(purrr::map(., ~ unique(as.character(.x[[rlang::as_string(uniquerow)]]))))



  makesets <- function(.data) {


    .data %>%
      tidyr::pivot_wider(
        id_cols = c({{ uniquerow }}, tidyselect::everything()),
        names_from = {{ choice }},
        values_from = c(n, perc),
        names_sep = "."
      )  %>%
      dplyr::select(- {{ uniquerow }}) %>%
      dplyr::rename_with(
        ~ gsub(paste0("^(", prefix, "(\\d+))_(.*)$"), "\\3.\\2", .),
        dplyr::matches(paste0("^", prefix, "\\d+_"))
      ) %>%
      tidyr::pivot_longer(
        cols = tidyselect::everything(),
        names_to = c("name", "suffix"),
        names_pattern = "(.*)\\.(.*)"
      ) %>%
      tidyr::pivot_wider(
        names_from = suffix,
        values_from = value
      )
  }

  finalsets <- purrr::map(sets, ~makesets(.x ))



  return(finalsets)

}

#finalsets2 <- createFreq(database, choice = "pref1", attributes = ends_with(c("ZEIT","x1")), uniquerow = "UniqueRow")
