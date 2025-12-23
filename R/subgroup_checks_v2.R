#' Harmonize subgroup labels (basic normalization) and check that each subgroup has >= 2 studies
#' @param df data.frame
#' @param subgroup_col column name (string) for subgroup
#' @param study_id column name for study id
#' @return list: harmonized_levels (table) and undersized_subgroups (tibble)
#' @export
check_subgroups_v2 <- function(df, subgroup_col, study_id = "study_id") {
  if (!(subgroup_col %in% names(df))) return(list(harmonized = tibble::tibble(), undersized = tibble::tibble()))
  # normalize subgroup strings
  tmp <- df
  tmp[[subgroup_col]] <- tmp[[subgroup_col]] %>% as.character() %>% stringr::str_trim() %>% stringr::str_to_title()
  harmonized <- tmp %>% dplyr::count(!!rlang::sym(subgroup_col)) %>% dplyr::arrange(desc(n))
  undersized <- tmp %>% dplyr::group_by(!!rlang::sym(subgroup_col)) %>% dplyr::summarise(n_studies = dplyr::n_distinct(!!rlang::sym(study_id))) %>% dplyr::filter(n_studies < 2)
  list(harmonized = harmonized, undersized = undersized)
}
