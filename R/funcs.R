# Function to capture and log messages
capture_messages <- function(expr) {
  # Create a text connection to capture output
  temp <- textConnection("messages", "w", local = TRUE)
  sink(temp, type = "message")
  on.exit({
    sink(type = "message")
    close(temp)
  })
  
  result <- expr
  
  # Get the captured messages
  if(exists("messages")) {
    current_log <- validation_log()
    new_msgs <- paste(messages, collapse = "\n")
    validation_log(paste0(current_log, if(nchar(current_log) > 0) "\n", new_msgs))
  }
  
  return(result)
}

# Raw read functions for each file type - mirrors the Excel import step of readMWR* without checks
raw_read_fns <- list(
  resdat = function(path) {
    suppressWarnings(readxl::read_excel(path, na = c('NA', 'na', ''), guess_max = Inf)) |>
      dplyr::mutate_if(function(x) !lubridate::is.POSIXct(x), as.character)
  },
  accdat = function(path) {
    dat <- readxl::read_excel(path, na = c('NA', ''), col_types = 'text')
    if ('Value Range' %in% names(dat))
      dat <- dplyr::mutate(dat, dplyr::across(-c(`Value Range`), ~ dplyr::na_if(.x, 'na')))
    dat
  },
  frecomdat = function(path) {
    suppressMessages(
      readxl::read_excel(path, skip = 1, na = c('NA', 'na', ''),
        col_types = c('text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
    ) |> dplyr::rename(`% Completeness` = `...7`)
  },
  sitdat = function(path) readxl::read_excel(path, na = c('NA', 'na', '')),
  wqxdat = function(path) suppressWarnings(readxl::read_excel(path, na = c('NA', 'na', ''), col_types = 'text'))
)

# Retry functions: run check + format on an edited data frame from handsontable
retry_fns <- list(
  resdat = function(df) {
    if ('Activity Start Date' %in% names(df) && !lubridate::is.POSIXct(df$`Activity Start Date`))
      df$`Activity Start Date` <- as.POSIXct(as.character(df$`Activity Start Date`))
    formMWRresults(checkMWRresults(df, warn = TRUE))
  },
  accdat = function(df) formMWRacc(checkMWRacc(df, warn = TRUE)),
  frecomdat = function(df) formMWRfrecom(checkMWRfrecom(df, warn = TRUE)),
  sitdat = function(df) checkMWRsites(df),
  wqxdat = function(df) formMWRwqx(checkMWRwqx(df, warn = TRUE))
)

# file upload for observers
fl_upload <- function(file, read_function, data_name) {
  req(file)
  validation_log("")
  edit_visible[[data_name]] <- FALSE
  raw_data_states[[data_name]] <- NULL

  result <- tryCatch({
    capture_messages(read_function(file$datapath))
  }, error = function(e) {
    validation_log(paste0("Error in ", data_name, ": ", e$message))
    raw <- tryCatch(raw_read_fns[[data_name]](file$datapath), error = function(e2) NULL)
    raw_data_states[[data_name]] <<- raw
    edit_visible[[data_name]] <<- !is.null(raw)
    NULL
  })

  data_states[[data_name]] <- result
}

# Handle retry after user edits in handsontable
# hot_headers_input: rhandsontable input for the column-name editor (may be NULL)
handle_retry <- function(data_name, hot_input, hot_headers_input = NULL) {
  req(hot_input)
  validation_log("")

  edited_df <- rhandsontable::hot_to_r(hot_input)

  # Apply any edited column names from the header editor
  if (!is.null(hot_headers_input)) {
    new_names <- unlist(rhandsontable::hot_to_r(hot_headers_input)[1, ], use.names = FALSE)
    if (length(new_names) == ncol(edited_df))
      names(edited_df) <- new_names
  }

  result <- tryCatch({
    capture_messages(retry_fns[[data_name]](edited_df))
  }, error = function(e) {
    validation_log(paste0("Error in ", data_name, ": ", e$message))
    NULL
  })

  data_states[[data_name]] <- result
  # Only hide the editor on success; leave it open (with edits intact) on failure
  if (!is.null(result)) {
    edit_visible[[data_name]] <- FALSE
    raw_data_states[[data_name]] <- NULL
    removeModal()
  }
}

# helper function to print file status in cards
fl_status <- function(tester, file_input, data_state) {
  if(tester) return(HTML("<span style='color:#4287f5'>Using test data</span>"))
  if(is.null(file_input)) return(HTML("No file uploaded"))
  if(is.null(data_state)) HTML("<span style='color:#f54242'>Error loading<span>") else HTML("<span style='color:#1e9c3b'>Data loaded</span>")
}

# dqo table theme
thmdqo <- function(x, dqofontsize, padding){
  flextable::colformat_double(x, na_str = '-') |> 
    flextable::colformat_char(na_str = '-') |> 
    flextable::border_inner() |> 
    flextable::align(align = 'center', part = 'all') |> 
    flextable::align(align = 'left', j = 1, part = 'all') |> 
    flextable::fontsize(size = dqofontsize, part = 'all') |> 
    flextable::padding(padding = padding, part = 'all')
}

# dqo summary table theme
thmsum <- function(x, wd){
  if(!is.null(x))
    flextable::width(x, width = wd / flextable::ncol_keys(x))
}

# frecomdat table
frecomdat_tab <- function(frecomdat, dqofontsize, padding, wd){

  frecomdat |> 
    dplyr::mutate_if(is.numeric, as.character) |> 
    dplyr::mutate_all(function(x) ifelse(is.na(x), '-', x)) |> 
    dplyr::arrange(Parameter, .locale = 'en') |> 
    flextable::flextable() |> 
    thmdqo(dqofontsize = dqofontsize, padding = padding) |>
    flextable::width(width = wd / ncol(frecomdat)) |>
    flextable::add_header_row(value = c('', 'Frequency %', ''), colwidths = c(1, 5, 1)) |> 
    flextable::set_caption("Frequency and Completeness") |> 
    flextable::htmltools_value()
  
}

# accdat table
accdat_tab <- function(accdat, dqofontsize, padding, wd){
  
  out <- accdat |> 
    dplyr::mutate_if(is.numeric, as.character) |> 
    dplyr::mutate_all(function(x) ifelse(is.na(x), '-', x)) |> 
    dplyr::arrange(Parameter, .locale = 'en') |> 
    flextable::flextable() |> 
    thmdqo(dqofontsize = dqofontsize, padding = padding) |> 
    flextable::width(width = 1, j = 1) 
  
  out <- out |> 
    flextable::width(width = (wd -1) / (flextable::ncol_keys(out) - 1), j = 2:flextable::ncol_keys(out)) |> 
    flextable::set_caption("Accuracy") |> 
    flextable::htmltools_value()
  
  return(out)
  
}