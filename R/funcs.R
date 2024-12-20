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

# file upload for observers
fl_upload <- function(file, read_function, data_name) {
  req(file)
  validation_log("")  # Clear previous messages
  
  data_states[[data_name]] <- tryCatch({
    capture_messages({
      # Use assign() to create the variable in the global environment
      assign(data_name, read_function(file$datapath), envir = .GlobalEnv)
    })
  }, error = function(e) {
    validation_log(paste0("Error in ", data_name, ": ", e$message))
    NULL
  })
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