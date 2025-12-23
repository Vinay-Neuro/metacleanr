#' Detect connected components (subnetworks) for NMA using igraph
#' @param df data.frame (long format)
#' @param study_id column name for study id
#' @param intervention column name for interventions
#' @return igraph components object (sizes, membership) or list with components
#' @export
detect_subnetworks <- function(df, study_id, intervention) {
  if (!all(c(study_id, intervention) %in% names(df))) return(NULL)
  edgelist <- df %>%
    dplyr::group_by(.data[[study_id]]) %>%
    dplyr::summarise(arms = list(unique(.data[[intervention]]))) %>%
    dplyr::pull(arms) %>%
    lapply(function(x) if (length(x) > 1) t(combn(x, 2)) else NULL) %>%
    unlist(recursive = FALSE)
  if (length(edgelist) == 0) return(NULL)
  el <- do.call(rbind, edgelist)
  g <- igraph::graph_from_edgelist(as.matrix(el))
  igraph::components(g)
}
