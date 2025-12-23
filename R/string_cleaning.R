#' Clean string columns: trim, unify case, remove punctuation (keeps alnum + spaces), unify spaces to _
#' @param df data.frame
#' @param cols character vector of column names to clean
#' @return data.frame
#' @export
clean_core_strings <- function(df, cols) {
  stopifnot(is.data.frame(df))
  for (c in cols) {
    if (!c %in% names(df)) next
    df[[c]] <- df[[c]] %>%
      as.character() %>%
      stringr::str_trim() %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all("[^a-z0-9 ]", "") %>%
      stringr::str_replace_all("\\s+", "_")
  }
  df
}
