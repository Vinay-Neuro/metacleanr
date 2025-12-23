#' Print method for metaclean reports
#' @param x object returned by metaclean
#' @export
print.metaclean_report <- function(x, ...) {
  cat("metacleanr report\n")
  if (!is.null(x$missing) && nrow(x$missing) > 0) cat(" - Missingness issues detected\n")
  if (!is.null(x$id_consistency) && nrow(x$id_consistency) > 0) cat(" - Study ID consistency issues detected\n")
  if (!is.null(x$outliers) && nrow(x$outliers) > 0) cat(" - Outliers detected\n")
  if (!is.null(x$invalid) && length(Filter(NROW, x$invalid)) > 0) cat(" - Invalid value issues detected\n")
  if (!is.null(x$intervention_variants) && nrow(x$intervention_variants) > 0) cat(" - Intervention variants detected\n")
  invisible(x)
}
