check_long_id_consistency <- function(df, study_id, arm_id) {
  df |>
    dplyr::group_by(.data[[arm_id]]) |>
    dplyr::summarise(n_ids = dplyr::n_distinct(.data[[study_id]])) |>
    dplyr::filter(n_ids > 1)
}
