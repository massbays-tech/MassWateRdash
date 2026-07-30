detect_wrong_file <- function(raw_df, data_name) {
  if (is.null(raw_df)) {
    return(NULL)
  }
  cols <- names(raw_df)
  if (any(file_signatures[[data_name]] %in% cols)) {
    return(NULL)
  }
  matches <- sapply(file_signatures, function(sigs) sum(sigs %in% cols))
  best_count <- max(matches)
  if (best_count == 0) {
    return(
      "Error: Did you upload the wrong file? The column names do not match the expected format."
    )
  }
  best <- names(which.max(matches))
  paste0(
    "Error: Did you upload the wrong file? This looks like it may be ",
    file_labels[[best]],
    "."
  )
}

# Raw read functions for each file type - mirrors the Excel import step of readMWR* without checks
raw_read_fns <- list(
  resdat = function(path) {
    suppressWarnings(
      readxl::read_excel(path, na = c("NA", "na", ""), guess_max = Inf)
    ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  },
  accdat = function(path) {
    dat <- readxl::read_excel(path, na = c("NA", ""), col_types = "text")
    if ("Value Range" %in% names(dat)) {
      dat <- dplyr::mutate(
        dat,
        dplyr::across(-c(`Value Range`), ~ dplyr::na_if(.x, "na"))
      )
    }
    dat
  },
  frecomdat = function(path) {
    suppressMessages(
      readxl::read_excel(
        path,
        skip = 1,
        na = c("NA", "na", ""),
        col_types = "text"
      )
    ) |>
      dplyr::rename(`% Completeness` = `...7`)
  },
  sitdat = function(path) readxl::read_excel(path, na = c("NA", "na", "")),
  wqxdat = function(path) {
    suppressWarnings(readxl::read_excel(
      path,
      na = c("NA", "na", ""),
      col_types = "text"
    ))
  },
  censdat = function(path) readxl::read_excel(path, na = c("NA", "na", ""))
)

# Retry functions: run check + format on an edited data frame from handsontable
retry_fns <- list(
  resdat = function(df) {
    if (
      "Activity Start Date" %in%
        names(df) &&
        !lubridate::is.POSIXct(df$`Activity Start Date`)
    ) {
      df$`Activity Start Date` <- as.POSIXct(
        as.character(df$`Activity Start Date`)
      )
    }
    formMWRresults(checkMWRresults(df, warn = TRUE))
  },
  accdat = function(df) formMWRacc(checkMWRacc(df, warn = TRUE)),
  frecomdat = function(df) formMWRfrecom(checkMWRfrecom(df, warn = TRUE)),
  sitdat = function(df) checkMWRsites(df),
  wqxdat = function(df) formMWRwqx(checkMWRwqx(df, warn = TRUE)),
  censdat = function(df) formMWRcens(checkMWRcens(df, warn = TRUE))
)

#' File upload
#'
#' @description `fl_upload()` ...
#'
#' @param file Input file
#' @param read_function Read function
#' @param data_name String. Data name.
#' @param val_log R6 class. Must contain function catch_msg and variables msg,
#' edit_dat.
#' @param val_edit R6 class. Contains `TRUE` and `FALSE` values on whether to
#' show/hide the edit modal.
#' @param val_dat R6 class. Must contain variables raw_dat, dat.
#'
#' @noRd
fl_upload <- function(
  file,
  read_function,
  data_name,
  val_log,
  val_edit,
  val_dat
) {
  req(file)

  val_log$msg <- ""
  val_dat$raw_dat <- NULL
  val_edit[[data_name]] <- FALSE

  dat_path <- if (is.character(file)) file else file$datapath # for testing

  result <- tryCatch(
    {
      val_log$catch_msg(read_function(dat_path))
    },
    error = function(e) {
      raw <- tryCatch(
        raw_read_fns[[data_name]](dat_path),
        error = function(e2) NULL
      )
      wrong_file_msg <- detect_wrong_file(raw, data_name)
      if (!is.null(wrong_file_msg)) {
        val_log$msg <- wrong_file_msg
      } else {
        val_log$msg <- paste0("Error in ", data_name, ": ", e$message)
        val_dat$raw_dat <- raw
        val_edit[[data_name]] <- !is.null(raw)
      }
      NULL
    }
  )

  val_dat$dat <- result
  val_dat$msg <- val_log$msg
}

#' From format upload
#'
#' @description `from_format_upload()` ...
#'
#' @param df Dataframe
#' @param retry_fn Function
#' @param data_name String. Data name.
#' @param val_log R6 class. Must contain function catch_msg and variables msg,
#' edit_dat.
#' @param val_edit R6 class. TRUE and FALSE values on whether to show/hide the
#' edits for each var
#' @param val_dat R6 class. Must contain variables raw_dat, dat.
#'
#' @noRd
from_format_upload <- function(
  df,
  retry_fn,
  data_name,
  val_log,
  val_edit,
  val_dat
) {
  val_log$msg <- ""
  val_dat$raw_dat <- NULL
  val_edit[[data_name]] <- FALSE

  result <- tryCatch(
    {
      val_log$catch_msg(retry_fn(df))
    },
    error = function(e) {
      val_log$msg <- paste0("Error processing ", data_name, ": ", e$message)
      val_dat$raw_dat <- df
      val_edit[[data_name]] <- TRUE
      NULL
    }
  )

  val_dat$dat <- result
  val_dat$msg <- val_log$msg
}

#' File status
#'
#' @description `fl_status` is a helper function to print file upload status in
#' cards.
#'
#' @param tester Boolean. If using test data, set to `TRUE`.
#' @param file_input Input data. Set to `NULL` if no data uploaded.
#' @param data_state String or dataframe. Set to `NULL` if error uploading data.
#'
#' @return HTML message
#'
#' @noRd
fl_status <- function(tester, file_input, data_state) {
  msg <- if (tester) {
    "<span style='color:#00A4CF'>Using test data</span>"
  } else if (is.null(file_input) && is.null(data_state)) {
    "No file uploaded"
  } else if (is.null(data_state)) {
    "<span style='color:#f54242'>Error loading</span>"
  } else if (is.null(file_input)) {
    "<span style='color:#64C147'>Loaded from format converter</span>"
  } else {
    "<span style='color:#64C147'>Data loaded</span>"
  }

  HTML(msg)
}

#' Format validation log
#'
#' @description `format_log()` formats a validation log as an HTML format.
#'
#' @param msg String. Input message.
#'
#' @return HTML message
#'
#' @noRd
format_log <- function(msg) {
  if (nchar(trimws(msg)) == 0) {
    return(NULL)
  }
  msg <- gsub("\033\\[[0-9;]*[mGKHFABCDJK]", "", msg) # strip ANSI codes
  lines <- strsplit(msg, "\n")[[1]]
  lines <- lines[nchar(trimws(lines)) > 0]
  div(HTML(paste(lines, collapse = "<br>")))
}
