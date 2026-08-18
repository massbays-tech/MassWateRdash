#' Check for column errors
#'
#' @description `is_column_error()` checks if the validation message contains
#' any warnings for missing or misnamed columns.
#'
#' @param msg String. Validation message.
#'
#' @return Boolean. If any column name warning(s) detected, returns `TRUE`,
#' else returns `FALSE`.
#'
#' @noRd
is_column_error <- function(msg) {
  if (is.null(msg) || nchar(trimws(msg)) == 0) {
    return(FALSE)
  }
  grepl("correct the column names|Missing the following columns", msg)
}

#' Parse problem rows
#'
#' @description `parse_problem_rows()` parses the listed row indices from a
#' validation message.
#'
#' @param msg String. Validation message.
#'
#' @return List or integer containing row numbers.
#'
#' @noRd
parse_problem_rows <- function(msg) {
  if (is.null(msg) || nchar(trimws(msg)) == 0) {
    return(integer(0))
  }
  msg <- gsub("\033\\[[0-9;]*[mGKHFABCDJK]", "", msg)
  hits <- regmatches(
    msg, gregexpr("row\\(s\\)\\s+([0-9, ]+)", msg, perl = TRUE)
  )[[1]]
  if (length(hits) == 0) {
    return(integer(0))
  }
  nums <- gsub("row\\(s\\)\\s+", "", hits)
  rows <- suppressWarnings(as.integer(unlist(strsplit(nums, "[, ]+"))))
  sort(unique(rows[!is.na(rows)]))
}

# Parse column indices and a column->row cell map from a validation message.
# col_indices: 1-based positions flagged via "(column N)" — for header highlighting.
# cell_map:    named list col_name -> row_indices for cell-level highlighting.
#   Pattern A: "ColName (row(s) N, M)" explicit pairing (frecomdat/accdat style).
#   Pattern B: a known column name appears in the same line as "row(s) N".
parse_error_locations <- function(msg, col_names = NULL) {
  empty <- list(col_indices = integer(0), cell_map = list())
  if (is.null(msg) || nchar(trimws(msg)) == 0) {
    return(empty)
  }
  msg <- gsub("\033\\[[0-9;]*[mGKHFABCDJK]", "", msg)

  col_idx_hits <- regmatches(
    msg, gregexpr("\\(column (\\d+)\\)", msg, perl = TRUE)
  )[[1]]
  col_indices <- suppressWarnings(
    as.integer(gsub("[^0-9]", "", col_idx_hits))
  ) |>
    unique() |>
    sort()
  col_indices <- col_indices[!is.na(col_indices)]

  cell_map <- list()

  for (ln in strsplit(msg, "\n")[[1]]) {
    if (!grepl("row\\(s\\)", ln)) {
      next
    }

    pA <- regmatches(
      ln,
      gregexpr("([^,\n]+?)\\s+\\(row\\(s\\)\\s+[\\d, ]+\\)", ln, perl = TRUE)
    )[[1]]
    if (length(pA) > 0) {
      for (hit in pA) {
        col_nm <- trimws(sub("\\s*\\(row\\(s\\).*", "", hit))
        rows_str <- regmatches(hit, regexpr("row\\(s\\)\\s+[\\d, ]+", hit))
        rows <- sort(unique(suppressWarnings(as.integer(
          unlist(strsplit(gsub("row\\(s\\)\\s+", "", rows_str), "[, ]+"))
        ))))
        rows <- rows[!is.na(rows)]
        if (nchar(col_nm) > 0 && length(rows) > 0) {
          cell_map[[col_nm]] <- sort(unique(c(cell_map[[col_nm]], rows)))
        }
      }
      next
    }

    if (!is.null(col_names)) {
      rows_str <- regmatches(
        ln, gregexpr("row\\(s\\)\\s+[\\d, ]+", ln, perl = TRUE)
      )[[1]]
      rows <- sort(unique(suppressWarnings(as.integer(
        unlist(strsplit(gsub("row\\(s\\)\\s+", "", rows_str), "[, ]+"))
      ))))
      rows <- rows[!is.na(rows)]
      if (length(rows) > 0) {
        for (cn in col_names) {
          if (grepl(cn, ln, fixed = TRUE)) {
            cell_map[[cn]] <- sort(unique(c(cell_map[[cn]], rows)))
          }
        }
      }
    }
  }

  list(col_indices = col_indices, cell_map = cell_map)
}

# Parse repeat errors
parse_repeat_errors <- function(dat, locs) {
  target_col <- names(locs$cell_map)[1]

  col_list <- c(
    "Parameter", "Characteristic Name", "uom", "Activity Depth/Height Unit",
    "Result Unit", "Activity Type"
  )

  if (is.null(target_col) || !target_col %in% col_list) {
    return(NULL)
  }

  problem_rows <- locs$cell_map[[target_col]]

  if (length(problem_rows) < 10) {
    return(NULL)
  }

  dat <- dat[problem_rows, , drop = FALSE]
  ndat <- dplyr::count(dat, .data[[target_col]])

  if (max(ndat$n) < 5) {
    return(NULL)
  }

  var_list <- if (target_col == "Activity Type") {
    c(
      "Field Msr/Obs", "Sample-Routine", "Quality Control Sample-Field Blank",
      "Quality Control Sample-Lab Duplicate",
      "Quality Control Sample-Lab Blank", "Quality Control Sample-Lab Spike",
      "Quality Control-Meter Lab Duplicate", "Quality Control-Meter Lab Blank",
      "Quality Control-Calibration Check"
    )
  } else if (target_col == "Activity Depth/Height Unit") {
    c("ft", "m")
  } else if (target_col %in% c("Parameter", "Characteristic Name")) {
    c(
      "Air Temp", "Algae, blue-green (phylum cyanophyta) density", "Ammonia",
      "Ammonium", "Chl a", "Chl a (probe)", "Chloride", "Chlorophyll a",
      "Chlorophyll a (probe)",
      "Chlorophyll a (probe) concentration, Cyanobacteria (bluegreen)",
      "Conductivity", "Cyanobacteria", "Cyanobacteria (probe)", "Depth",
      "Depth, Secchi disk depth", "Dissolved oxygen (DO)",
      "Dissolved oxygen saturation", "DO", "DO saturation", "E.coli",
      "Enterococcus", "Escherichia coli", "Fecal Coliform", "Flow", "Gage",
      "Height, gage", "Metals", "Microcystins", "Nitrate", "Nitrate + Nitrite",
      "Nitrite", "Ortho P", "Orthophosphate", "Particulate organic carbon",
      "pH", "Pheophytin", "Pheophytin a", "Phosphorus, Particulate Organic",
      "Phycocyanin", "Phycocyanin (probe)", "Phycoerythrin", "POC", "PON",
      "POP", "Salinity", "Secchi Depth", "Silicate", "Sp Conductance",
      "Specific conductance", "Sulfate", "Surfactants", "TDN", "TDP", "TDS",
      "Temperature, air", "Temperature, water", "TKN", "TN",
      "Total dissolved solids", "Total Kjeldahl nitrogen",
      "Total Nitrogen, mixed forms", "Total Phosphorus, mixed forms",
      "Total suspended solids", "TP", "TSS", "Turbidity", "Water Temp"
    )
  } else {
    c(
      "#/100ml", "%", "% recovery", "AU", "BU", "cfm", "cfs", "cfu/100ml", "cm",
      "deg C", "deg F", "FAU", "FBU", "FNMU", "FNRU", "FNU", "ft", "FTU",
      "g/kg", "JTU", "l/min", "l/sec", "m", "mg/l", "mgd", "MPN/100ml", "mS/cm",
      "None", "NTMU", "NTRU", "NTU", "ppm", "ppt", "ppth", "PSS", "PSU", "RFU",
      "s.u.", "S/m", "ug/l", "umol/l", "uS/cm"
    )
  }
  var_list <- c(" ", var_list)

  new_col <- paste("Invalid", target_col)

  ndat <- ndat |>
    dplyr::filter(.data$n > 1) |>
    dplyr::rename(!!new_col := !!target_col, "Row Count" = "n") |>
    dplyr::mutate("Delete Rows" = FALSE, .before = !!new_col) |>
    dplyr::mutate(
      "Replace With" = factor(NA, levels = !!var_list),
      .before = "Row Count"
    )

  rhandsontable::rhandsontable(ndat, width = "100%", height = 450) |>
    rhandsontable::hot_table(wordWrap = FALSE) |>
    rhandsontable::hot_col(new_col, readOnly = TRUE) |>
    rhandsontable::hot_col("Row Count", readOnly = TRUE)
}

# Splitting handle retry in to multiple functions
# Function 1: update column names
update_hot_col <- function(val_dat, hot_table) {
  edited_df <- val_dat$raw_dat

  new_names <- unlist(
    rhandsontable::hot_to_r(hot_table)[1, ],
    use.names = FALSE
  )
  if (length(new_names) == ncol(edited_df)) {
    names(edited_df) <- new_names
  }

  edited_df
}

# Function 2: update variables (for mass edits)
update_hot_var <- function(val_dat, hot_table) {
  dat_var <- rhandsontable::hot_to_r(hot_table) |>
    dplyr::filter(
      .data[["Delete Rows"]] == TRUE || .data[["Replace With"]] != " "
    )

  if (nrow(dat_var) == 0) {
    return(NULL)
  }

  edited_dat <- val_dat$raw_dat

  var_col <- colnames(dat_var)[2]
  target_col <- gsub("Invalid ", "", var_col)

  del_list <- dat_var |>
    dplyr::filter(.data[["Delete Rows"]] == TRUE) |>
    dplyr::pull(.data[[var_col]])

  if (length(del_list) > 0) {
    edited_dat <- edited_dat |>
      dplyr::filter(!.data[[target_col]] %in% del_list)
  }

  dat_sub <- dat_var |>
    dplyr::filter(.data[["Delete Rows"]] == FALSE)

  if (nrow(dat_sub) == 0) {
    return(edited_dat)
  }

  old_var <- dat_sub[, 2]
  new_var <- dat_sub[, 3]

  wqformat::update_var(edited_dat, target_col, old_var, new_var)
}

# Fucntion 3: update individual rows
# show_all: TRUE when the user toggled to full-table view (no row merge needed)
# problem_rows: indices that were displayed in filtered view
update_hot_row <- function(val_dat, hot_table, show_all = TRUE, problem_rows = integer(0)) {
  edited_df <- rhandsontable::hot_to_r(hot_table)

  # If filtered view was active, merge the edited subset back into the full data
  if (!show_all && length(problem_rows) > 0 && !is.null(val_dat$raw_dat)) {
    full_df <- val_dat$raw_dat
    names(full_df) <- names(edited_df)
    valid_rows <- problem_rows[
      problem_rows >= 1 & problem_rows <= nrow(full_df)
    ]
    full_df[valid_rows, ] <- edited_df[seq_along(valid_rows), ]
    edited_df <- full_df
  }

  # Drop blank rows
  edited_df[!apply(is.na(edited_df) | edited_df == "", 1, all), ]
}


# Handle retry after user edits in handsontable
# .data -- edited_df from previous function, but pipeable
handle_retry <- function(.data, data_name, val_log, val_edit, val_dat) {
  val_log$msg <- ""
  edited_df <- .data

  # Persist edits into raw_data_states so they survive a failed retry and the
  # re-rendered table reflects the user's work on the next round of checks
  val_dat$raw_dat <- edited_df

  result <- tryCatch(
    {
      val_log$catch_msg(retry_fns[[data_name]](edited_df))
    },
    error = function(e) {
      val_log$msg <- paste0("Error in ", data_name, ": ", e$message)
      NULL
    }
  )

  val_dat$dat <- result
  val_dat$msg <- val_log$msg

  if (!is.null(result)) {
    val_edit[[data_name]] <- FALSE
    val_dat$raw_dat <- NULL
  }
}
