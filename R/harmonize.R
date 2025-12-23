#' Suggest an intervention harmonization map from clustered variants
#'
#' @param variant_tbl tibble with columns name and group as returned by detect_name_variants()
#' @param data original data.frame (used to count frequencies)
#' @param intervention_col name of the intervention column in data (string)
#' @param strategy how to pick representative label: "mode" (default) or "shortest"
#' @return tibble with columns: original, suggested, group, count
#' @export
suggest_intervention_map <- function(variant_tbl, data = NULL, intervention_col = NULL, strategy = c("mode", "shortest")) {
  strategy <- match.arg(strategy)
  stopifnot("name" %in% names(variant_tbl) && "group" %in% names(variant_tbl))
  uniqs <- variant_tbl$name
  groups <- variant_tbl$group
  
  # If data + intervention_col provided, compute counts to help choose the most frequent label
  counts <- NULL
  if (!is.null(data) && !is.null(intervention_col) && intervention_col %in% names(data)) {
    counts <- as.data.frame(table(data[[intervention_col]]), stringsAsFactors = FALSE)
    names(counts) <- c("name", "count")
  } else {
    counts <- data.frame(name = uniqs, count = 0, stringsAsFactors = FALSE)
  }
  
  df <- dplyr::tibble(name = uniqs, group = groups) %>%
    dplyr::left_join(counts, by = "name") %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(
      suggested = if (strategy == "mode") {
        # choose the most frequent label (if tie, choose alphabetically smallest)
        group_names <- name
        freq_tbl <- table(name)
        group_names[which.max(freq_tbl[group_names])]
      } else {
        # choose the shortest label (heuristic)
        name[which.min(nchar(name))]
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(original = name, suggested, group, count)
  
  # If suggested is NA (rare), fall back to original name
  df$suggested[is.na(df$suggested)] <- df$original[is.na(df$suggested)]
  df
}

#' Apply an intervention mapping to a dataset
#'
#' @param data data.frame
#' @param intervention_col name of the intervention column (string)
#' @param map tibble with columns original and suggested (as returned by suggest_intervention_map)
#' @return data.frame with intervention values harmonized (and attribute metaclean_fixes updated)
#' @export
apply_intervention_map <- function(data, intervention_col, map) {
  stopifnot(is.data.frame(data))
  stopifnot(intervention_col %in% names(data))
  stopifnot(all(c("original", "suggested") %in% names(map)))
  
  # Build mapping vector
  m <- setNames(as.character(map$suggested), map$original)
  
  # Replace values preserving NAs
  data[[intervention_col]] <- vapply(data[[intervention_col]], function(v) {
    if (is.na(v)) return(NA_character_)
    v_chr <- as.character(v)
    if (v_chr %in% names(m)) return(m[[v_chr]])
    return(v_chr)
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  
  # Add log attribute
  existing_log <- attr(data, "metaclean_fixes")
  entry <- paste0(Sys.time(), " - Applied intervention_map to column '", intervention_col, "'.")
  attr(data, "metaclean_fixes") <- c(existing_log, entry)
  
  data
}
