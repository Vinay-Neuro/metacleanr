#' Improved type inference and extracted_type annotation
#'
#' Tries to coerce character columns to numeric safely. Also creates companion columns
#' describing the "extracted_type" for value columns (e.g., mean -> mean_type).
#'
#' @param df data.frame
#' @param value_cols character vector of the main value columns to annotate (e.g., c("mean","sd","n","events"))
#' @param na_increase_threshold numeric proportion of allowed NA increase when coercing (default 0.1)
#' @return data.frame with coerced columns and new companion columns like mean_type
#' @export
infer_column_types_v2 <- function(df, value_cols = NULL, na_increase_threshold = 0.10) {
  stopifnot(is.data.frame(df))
  # Basic: attempt to coerce character -> numeric safely across all columns
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      suppressWarnings(num <- as.numeric(stringr::str_replace_all(df[[col]], ",", "")))
      na_prev <- mean(is.na(df[[col]]))
      na_new <- mean(is.na(num))
      if (!is.na(na_new) && (na_new - na_prev) <= na_increase_threshold) {
        df[[col]] <- num
        # mark attribute that column was coerced
        attr(df[[col]], "coerced_from") <- "character_to_numeric"
      } else {
        # leave as character; owner will be flagged later as invalid for numeric columns
      }
    }
  }
  
  # Create extracted_type columns for each requested value column
  if (!is.null(value_cols)) {
    for (v in value_cols) {
      if (!v %in% names(df)) next
      type_col <- paste0(v, "_type")
      df[[type_col]] <- vapply(seq_len(nrow(df)), function(i) {
        val <- df[[v]][i]
        # NA is NA
        if (is.na(val)) return("missing")
        # if a character original (we can't easily tell now) use heuristic:
        if (is.character(val)) {
          # try common median indicators: "median" column presence is better handled separately
          return("string_non_numeric")
        }
        # numeric values: mark raw numeric
        if (is.numeric(val)) return("raw_numeric")
        return("other")
      }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    }
  }
  df
}
