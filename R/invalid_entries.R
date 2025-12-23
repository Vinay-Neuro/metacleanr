#' Detect invalid entries (non-numeric in numeric cols, events>n, decimals in counts)
#' @param df data.frame
#' @param values list with keys n and optionally events, mean, sd
#' @return list with tibbles for each issue type
#' @export
detect_invalid_entries <- function(df, values = list(n = "n", events = "events", mean = "mean", sd = "sd")) {
  issues <- list()
  ncol <- values$n
  evcol <- values$events
  
  # 1) non-numeric in numeric columns (where column is expected numeric)
  numeric_expected <- intersect(c(values$n, values$mean, values$sd, values$events), names(df))
  non_numeric <- list()
  for (col in numeric_expected) {
    # find rows where class is not numeric or NA but cannot coerce
    bad_rows <- which(!is.na(df[[col]]) & !is.numeric(df[[col]]))
    if (length(bad_rows) > 0) {
      non_numeric[[col]] <- df[bad_rows, , drop = FALSE]
    } else {
      # also check for coercion failures represented as NA after numeric coercion
      if (!is.numeric(df[[col]])) {
        # attempt coercion to detect failure
        suppressWarnings(num <- as.numeric(df[[col]]))
        failed <- which(!is.na(df[[col]]) & is.na(num))
        if (length(failed) > 0) non_numeric[[col]] <- df[failed, , drop = FALSE]
      }
    }
  }
  issues$non_numeric <- non_numeric
  
  # 2) events > n
  if (!is.null(evcol) && evcol %in% names(df) && ncol %in% names(df)) {
    issues$events_gt_n <- df %>%
      dplyr::filter(!is.na(.data[[evcol]]) & !is.na(.data[[ncol]]) & (.data[[evcol]] > .data[[ncol]]))
  } else {
    issues$events_gt_n <- tibble::tibble()
  }
  
  # 3) decimals in n or events where counts expected
  decimal_n <- tibble::tibble()
  if (ncol %in% names(df) && is.numeric(df[[ncol]])) {
    decimal_n <- df %>%
      dplyr::filter(!is.na(.data[[ncol]]) & (abs(.data[[ncol]] - round(.data[[ncol]])) > .Machine$double.eps ^ 0.5))
  }
  issues$decimal_n <- decimal_n
  
  if (!is.null(evcol) && evcol %in% names(df) && is.numeric(df[[evcol]])) {
    issues$decimal_events <- df %>%
      dplyr::filter(!is.na(.data[[evcol]]) & (abs(.data[[evcol]] - round(.data[[evcol]])) > .Machine$double.eps ^ 0.5))
  } else {
    issues$decimal_events <- tibble::tibble()
  }
  
  issues
}
