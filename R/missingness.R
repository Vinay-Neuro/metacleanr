#' Check missingness aggregated by study
#' @param df data.frame
#' @param study_id study id column name
#' @return tibble of studies with any missing values (TRUE = missing in at least one column)
#' @export
check_missing_by_study <- function(df, study_id) {
  stopifnot(is.data.frame(df))
  if (!study_id %in% names(df)) stop("study_id not found")
  df %>%
    dplyr::group_by(.data[[study_id]]) %>%
    dplyr::summarise(dplyr::across(everything(), ~ any(is.na(.)))) %>%
    dplyr::filter(if_any(-dplyr::all_of(study_id), identity))
}
