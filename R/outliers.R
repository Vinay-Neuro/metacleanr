#' Detect outliers in sample size (n) using robust IQR method
#' @param df data.frame
#' @param n_col column name (string) for sample size
#' @return tibble of rows flagged as outliers
#' @export
detect_outliers_meta <- function(df, n_col) {
  if (!n_col %in% names(df)) return(tibble::tibble())
  vec <- df[[n_col]]
  if (!is.numeric(vec)) return(tibble::tibble())
  q <- stats::quantile(vec, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower <- q[1] - 3 * iqr
  upper <- q[2] + 3 * iqr
  df %>% dplyr::mutate(.outlier_n = vec < lower | vec > upper) %>% dplyr::filter(.outlier_n)
}
