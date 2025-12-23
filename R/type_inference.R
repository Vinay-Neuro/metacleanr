#' Infer and coerce column types where safe
#'
#' Attempts numeric coercion on character columns. If the coercion introduces few new NAs,
#' the column is converted to numeric.
#' @param df data.frame
#' @param threshold proportion of NA increase allowed (default 0.1)
#' @return data.frame with coerced columns where appropriate
#' @export
infer_column_types <- function(df, threshold = 0.1) {
  stopifnot(is.data.frame(df))
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      suppressWarnings(num <- as.numeric(df[[col]]))
      na_prev <- mean(is.na(df[[col]]))
      na_new <- mean(is.na(num))
      na_diff <- na_new - na_prev
      if (!is.na(na_diff) && na_diff <= threshold) {
        df[[col]] <- num
      }
    }
  }
  df
}
