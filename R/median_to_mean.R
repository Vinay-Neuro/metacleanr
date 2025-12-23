#' Convert median + IQR to mean and sd (conservative approximation)
#'
#' This function implements a commonly used approximation for converting median and IQR (q1,q3)
#' to an estimated mean and sd for meta-analysis when only median and IQR are reported.
#' The method used here is a simple and conservative approximation:
#'   mean_est = (q1 + median + q3) / 3
#'   sd_est   = (q3 - q1) / 1.35
#'
#' These are heuristic approximations (widely used in applied meta-analyses) and should be
#' cited in publications. More sophisticated methods (Wan et al. 2014; Luo et al. 2018) exist
#' and can be implemented later. Use with caution and report that conversion was applied.
#'
#' @param median numeric vector
#' @param q1 numeric vector (first quartile)
#' @param q3 numeric vector (third quartile)
#' @return tibble with mean_est and sd_est (NA for rows without required inputs)
#' @export
median_to_mean_wanish <- function(median, q1, q3) {
  n <- length(median)
  mean_est <- rep(NA_real_, n)
  sd_est <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (!is.na(median[i]) && !is.na(q1[i]) && !is.na(q3[i])) {
      mean_est[i] <- (q1[i] + median[i] + q3[i]) / 3
      sd_est[i] <- (q3[i] - q1[i]) / 1.35
    }
  }
  tibble::tibble(mean_est = mean_est, sd_est = sd_est)
}
