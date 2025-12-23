# Robust detect_subnetworks replacement
# Requires: dplyr, igraph (both are common deps; adjust imports accordingly)
detect_subnetworks <- function(data, study_id, intervention) {
  # defensive checks
  if (missing(data) || missing(study_id) || missing(intervention)) {
    stop("detect_subnetworks requires data, study_id and intervention arguments.")
  }
  # ensure columns exist
  if (!all(c(study_id, intervention) %in% names(data))) {
    stop("detect_subnetworks: specified study_id or intervention column not found in data.")
  }

  # minimal tidy extraction (avoid coupling to tibble semantics)
  df <- data[, c(study_id, intervention), drop = FALSE]
  # coerce to character for safe comparisons
  df[[study_id]] <- as.character(df[[study_id]])
  df[[intervention]] <- ifelse(is.na(df[[intervention]]), NA_character_,
                               trimws(as.character(df[[intervention]])))

  # split by study
  studies <- split(df, df[[study_id]], drop = TRUE)

  # build per-study edges (pairs of interventions within each study)
  edgelist <- lapply(names(studies), function(study_name) {
    st <- studies[[study_name]]
    trts <- unique(na.omit(st[[intervention]]))
    if (length(trts) < 2L) {
      # return NULL for single-arm (we will compact these out)
      return(NULL)
    }
    # Use combn to form all unordered pairs
    if (length(trts) == 2L) {
      combos <- matrix(trts, nrow = 1)
      colnames(combos) <- NULL
    } else {
      combos <- t(combn(trts, 2))
    }
    # create data.frame with study and treatment pairs
    data.frame(
      study = rep(study_name, nrow(combos)),
      t1 = combos[, 1],
      t2 = combos[, 2],
      stringsAsFactors = FALSE
    )
  })

  # remove NULLs produced by single-arm or invalid studies
  # base-R variant so it doesn't require purrr
  if (length(edgelist) == 0L) {
    stop("detect_subnetworks: no studies to process.")
  }
  non_null_idx <- vapply(edgelist, function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0L, logical(1))
  edgelist_clean <- edgelist[non_null_idx]

  if (length(edgelist_clean) == 0L) {
    stop("detect_subnetworks: no valid edges could be constructed. Check that studies have >= 2 arms and interventions are present.")
  }

  # safe rbind
  edgelist_df <- do.call(rbind, edgelist_clean)
  # normalize column names
  names(edgelist_df)[names(edgelist_df) == "t1"] <- "from"
  names(edgelist_df)[names(edgelist_df) == "t2"] <- "to"

  # create graph and detect connected components (subnetworks)
  # handle if igraph not installed
  if (!requireNamespace("igraph", quietly = TRUE)) {
    warning("igraph not available: returning edgelist without subnetworks. Install igraph for subnetwork detection.")
    return(list(
      edgelist = edgelist_df,
      subnetworks = NULL
    ))
  }

  g <- igraph::graph_from_data_frame(d = edgelist_df[, c("from", "to")], directed = FALSE)
  comps <- igraph::components(g)

  # map treatments to component id
  comp_df <- data.frame(
    treatment = names(comps$membership),
    component = as.integer(comps$membership),
    stringsAsFactors = FALSE
  )

  # build list of subnetworks where each element is the treatments in that component
  subnetworks <- split(comp_df$treatment, comp_df$component)

  return(list(
    edgelist = edgelist_df,
    subnetworks = subnetworks,
    component_membership = comp_df
  ))
}
