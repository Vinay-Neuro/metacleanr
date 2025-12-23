#' metaclean: validate and harmonize meta-analysis datasets
#'
#' Main wrapper. Users call this once to run validations and get a report.
#'
#' @param data data.frame or tibble
#' @param format "long" or "wide"
#' @param meta_type "pairwise" or "nma"
#' @param outcome "MD","RR","OR","RAW"
#' @param study_id column name for study id (string)
#' @param arm_id column name for arm id when format="long" (string)
#' @param intervention string (for long) or vector (for wide) of intervention column names
#' @param values list with named entries for value columns (see docs)
#' @param subgroup optional subgroup column name or vector
#' @param fix one of "none", "suggest", "apply" — controls automatic suggestion / application of safe fixes
#' @param apply_median_conversion logical; if TRUE and median+q1+q3 columns are supplied in `values`, conversions will be applied when fix="apply"
#' @return an object (list) with elements: data, report, meta_type, outcome
#' @export
metaclean <- function(
    data,
    format = c("long", "wide"),
    meta_type = c("pairwise", "nma"),
    outcome = c("MD", "RR", "OR", "RAW"),
    study_id,
    arm_id = NULL,
    intervention,
    values,
    subgroup = NULL,
    fix = c("none", "suggest", "apply"),
    apply_median_conversion = FALSE
) {

  format <- match.arg(format)
  meta_type <- match.arg(meta_type)
  outcome <- match.arg(outcome)
  fix <- match.arg(fix)

  # basic cleaning steps (lossless/safe)
  data <- janitor::clean_names(data)
  # use v2 inference if available (creates *_type columns)
  if (exists("infer_column_types_v2", where = "package:metacleanr") || exists("infer_column_types_v2")) {
    data <- infer_column_types_v2(data, value_cols = c(values$mean, values$sd, values$n, values$events, values$median, values$q1, values$q3))
  } else {
    data <- infer_column_types(data)
  }

  cols_to_clean <- c(study_id, if (!is.null(arm_id)) arm_id else NULL,
                     if (!is.null(intervention) && is.character(intervention)) intervention else NULL,
                     subgroup)
  cols_to_clean <- unique(na.omit(cols_to_clean))
  data <- clean_core_strings(data, cols_to_clean)

  # If wide, reshape to internal long canonical format
  if (format == "wide") {
    data <- reshape_wide_to_long(data, study_id, intervention, values)
    arm_id <- "arm"
  }

  # initial diagnostics (flags only)
  report <- list()
  report$missing <- check_missing_by_study(data, study_id)
  report$id_consistency <- check_long_id_consistency(data, study_id, arm_id)
  report$outliers <- detect_outliers_meta(data, values[[ "n" ]])
  report$invalid <- detect_invalid_entries(data, values = values)

  # intervention variants detection (clusters)
  intervention_col <- if (is.character(intervention)) as.character(intervention[1]) else stop("intervention must be character name in long format")
  report$intervention_variants <- detect_name_variants(data[[intervention_col]])

  # Suggest or apply harmonization if requested
  report$suggestions <- list()
  report$applied_fixes <- list()

  if (fix == "suggest") {
    report$suggestions$intervention_map <- suggest_intervention_map(report$intervention_variants, data, intervention_col)
  } else if (fix == "apply") {
    # create suggestion map and apply it automatically
    map <- suggest_intervention_map(report$intervention_variants, data, intervention_col)
    data <- apply_intervention_map(data, intervention_col, map)
    report$suggestions$intervention_map <- map
    report$applied_fixes$intervention_map <- map
    existing_log <- attr(data, "metaclean_fixes")
    entry <- paste0(Sys.time(), " - Applied intervention harmonization (auto) using Jaro-Winkler clustering.")
    attr(data, "metaclean_fixes") <- c(existing_log, entry)
  }

  # suggestions for coercion / median->mean when in suggest mode
  if (fix == "suggest") {
    # coercion suggestions (simple)
    coercion_suggestions <- lapply(names(data), function(col) {
      if (is.character(data[[col]])) {
        suppressWarnings(num <- as.numeric(stringr::str_replace_all(data[[col]], ",", "")))
        na_prev <- mean(is.na(data[[col]])); na_new <- mean(is.na(num))
        if (!is.na(na_new) && (na_new - na_prev) <= 0.1) {
          return(tibble::tibble(column = col, suggested_action = "coerce_to_numeric", na_increase = na_new - na_prev))
        }
      }
      NULL
    })
    report$suggestions$coercion <- dplyr::bind_rows(coercion_suggestions)

    # median->mean suggestions if median,q1,q3 exist in values
    if (!is.null(values$median) && !is.null(values$q1) && !is.null(values$q3) &&
        all(c(values$median, values$q1, values$q3) %in% names(data))) {
      rows_to_convert <- which(!is.na(data[[values$median]]) & !is.na(data[[values$q1]]) & !is.na(data[[values$q3]]))
      if (length(rows_to_convert) > 0) {
        report$suggestions$median_to_mean_rows <- tibble::tibble(row = rows_to_convert, reason = "has median, q1, q3")
      }
    }
  }

  # apply median->mean conversion only when fix="apply" AND apply_median_conversion==TRUE
  if (fix == "apply" && isTRUE(apply_median_conversion)) {
    if (!is.null(values$median) && !is.null(values$q1) && !is.null(values$q3) &&
        all(c(values$median, values$q1, values$q3) %in% names(data))) {
      rows_to_convert <- which(!is.na(data[[values$median]]) & !is.na(data[[values$q1]]) & !is.na(data[[values$q3]]))
      if (length(rows_to_convert) > 0) {
        conv <- median_to_mean_wanish(as.numeric(data[[values$median]][rows_to_convert]),
                                      as.numeric(data[[values$q1]][rows_to_convert]),
                                      as.numeric(data[[values$q3]][rows_to_convert]))
        # backup originals
        data[[paste0(values$mean, "_orig")]] <- data[[values$mean]]
        data[[paste0(values$sd, "_orig")]] <- data[[values$sd]]
        # apply conversions into mean and sd columns (coerce to numeric)
        data[[values$mean]][rows_to_convert] <- conv$mean_est
        data[[values$sd]][rows_to_convert] <- conv$sd_est
        # document what happened
        report$applied_fixes$median_to_mean <- tibble::tibble(row = rows_to_convert, method = "median_to_mean_wanish", date = Sys.time())
        attr(data, "metaclean_fixes") <- c(attr(data, "metaclean_fixes"), paste0(Sys.time(), " - median->mean conversion applied to ", length(rows_to_convert), " rows."))
      }
    }
  }

  if (!is.null(subgroup)) {
    report$subgroup <- check_subgroups_v2(data, subgroup, study_id)
  }

  if (meta_type == "nma") {
    report$nma <- detect_subnetworks(data, study_id, intervention = intervention_col)
  }

  # open the cleaned data for inspection (non-blocking)
  try(View(data), silent = TRUE)

  class(report) <- "metaclean_report"

  return(list(data = data, report = report, meta_type = meta_type, outcome = outcome))
}
