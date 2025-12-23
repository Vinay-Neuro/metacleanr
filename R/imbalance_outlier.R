#' Detect outliers in numeric columns using IQR and MAD and detect arm imbalance
#' @param df data.frame
#' @param n_col name of sample size column
#' @param mean_col mean column name (optional)
#' @param sd_col sd column name (optional)
#' @param imbalance_threshold numeric ratio (max/min) to flag imbalance (default 10)
#' @return list with outlier tibbles and imbalance tibble
#' @export
detect_outliers_and_imbalance <- function(df, n_col = "n", mean_col = NULL, sd_col = NULL, imbalance_threshold = 10) {
  out <- list()
  # IQR-based for n
  if (n_col %in% names(df) && is.numeric(df[[n_col]])) {
    q <- stats::quantile(df[[n_col]], c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lower <- q[1] - 3 * iqr; upper <- q[2] + 3 * iqr
    out$outlier_n <- df %>% dplyr::filter(!is.na(.data[[n_col]]) & (.data[[n_col]] < lower | .data[[n_col]] > upper))
  } else out$outlier_n <- tibble::tibble()
  
  # MAD-based for mean / sd if provided
  if (!is.null(mean_col) && mean_col %in% names(df) && is.numeric(df[[mean_col]])) {
    med <- stats::median(df[[mean_col]], na.rm = TRUE)
    madv <- stats::mad(df[[mean_col]], na.rm = TRUE)
    out$outlier_mean <- df %>% dplyr::filter(!is.na(.data[[mean_col]]) & (abs(.data[[mean_col]] - med) > 3 * madv))
  } else out$outlier_mean <- tibble::tibble()
  
  if (!is.null(sd_col) && sd_col %in% names(df) && is.numeric(df[[sd_col]])) {
    med <- stats::median(df[[sd_col]], na.rm = TRUE)
    madv <- stats::mad(df[[sd_col]], na.rm = TRUE)
    out$outlier_sd <- df %>% dplyr::filter(!is.na(.data[[sd_col]]) & (abs(.data[[sd_col]] - med) > 3 * madv))
  } else out$outlier_sd <- tibble::tibble()
  
  # imbalance check per study: ratio of max n to min n
  if (n_col %in% names(df)) {
    out$imbalance <- df %>%
      dplyr::group_by(study_id = .data$study_id) %>%
      dplyr::summarise(max_n = max(.data[[n_col]], na.rm = TRUE), min_n = min(.data[[n_col]], na.rm = TRUE), ratio = dplyr::if_else(min_n > 0, max_n / min_n, NA_real_)) %>%
      dplyr::filter(!is.na(ratio) & ratio > imbalance_threshold)
  } else out$imbalance <- tibble::tibble()
  
  out
}
