#' Detect likely variants in intervention labels using Jaro-Winkler distance + clustering
#' @param x character vector of intervention labels
#' @param threshold numeric (0-1) height to cut tree; lower -> stricter grouping
#' @return tibble with name and cluster id
#' @export
detect_name_variants <- function(x, threshold = 0.15) {
  x <- unique(na.omit(as.character(x)))
  if (length(x) <= 1) return(tibble::tibble(name = x, group = seq_along(x)))
  dmat <- stringdist::stringdistmatrix(x, x, method = "jw")
  hc <- stats::hclust(as.dist(dmat), method = "average")
  groups <- cutree(hc, h = threshold)
  tibble::tibble(name = x, group = groups) %>% dplyr::arrange(group, name)
}
