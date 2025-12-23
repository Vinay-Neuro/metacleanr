#' Check for common invalid data entries (events > n, decimals where integers expected)
#' @param df data.frame
#' @param values list with names 'n' and optionally 'events'
#' @return list of tibbles with problems
#' @export
check_invalid_values <- function(df, values) {
  issues <- list()
  if (!is.null(values$events) && values$events %in% names(df) && values$n %in% names(df)) {
    issues$events_gt_n <- df %>% dplyr::filter(!is.na(.data[[values$events]]) & !is.na(.data[[values$n]]) & (.data[[values$events]] > .data[[values$n]]))
  } else {
    issues$events_gt_n <- tibble::tibble()
  }
  # decimals in n or events
  issues$decimal_n <- if (values$n %in% names(df)) df %>% dplyr::filter(!is.na(.data[[values$n]]) & (abs(.data[[values$n]] - round(.data[[values$n]])) > .Machine$double.eps ^ 0.5)) else tibble::tibble()
  if (!is.null(values$events) && values$events %in% names(df)) {
    issues$decimal_events <- df %>% dplyr::filter(!is.na(.data[[values$events]]) & (abs(.data[[values$events]] - round(.data[[values$events]])) > .Machine$double.eps ^ 0.5))
  } else {
    issues$decimal_events <- tibble::tibble()
  }
  issues
}
