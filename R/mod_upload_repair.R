#' Repair upload errors UI
#'
#' @description `mod_upload_repair_ui()` is a helper module for
#' `mod_upload_ui()`. It lets the user edit columns and variables in an
#' interactive process.
#'
#' @param id Namespace id for module. Should match `mod_upload_repair_server()`
#' id.
#' @param dat_name String. Short dataframe name.
#'
#' @noRd
mod_upload_repair_ui <- function(id, dat_name) {
  ns <- NS(id)

  tagList(
    conditionalPanel(
      condition = paste0('output["', ns("show_btn"), '"] == "', dat_name, '"'),
      actionButton(
        ns("open_editor"),
        paste("Edit", unname(file_labels[dat_name])),
        class = "btn-warning",
        icon = icon("pencil")
      )
    )
  )
}

#' Repair upload errors SERVER
#'
#' @description `mod_upload_repair_server()` is a helper module for
#' `mod_upload_server()`. It lets the user edit columns and variables in an
#' interactive process.
#'
#' @param id Namespace id for module. Should match `mod_upload_repair_ui()` id.
#' @param dat_name String. Short dataframe name.
#' @param val_log R6 class. Validation log and related functions.
#' @param val_dat R6 class. Dataframes.
#'
#' @noRd
mod_upload_repair_server <- function(
  id,
  dat_name,
  val_log,
  val_dat
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Toggle edit button visibility
    output$show_btn <- renderText({
      paste(val_log$edit_dat)
    })
    outputOptions(output, "show_btn", suspendWhenHidden = FALSE)

    # Create modal ----
    observe({
      showModal(
        modalDialog(
          title = paste(
            "Edit",
            unname(file_labels[dat_name]),
            "- Fix Validation Errors"
          ),
          size = "xl",
          easyClose = FALSE,
          uiOutput(ns("modal_msgs")),
          br(),
          p("Fix the issue below, then click 'Try upload again'."),
          if (is_column_error(val_log$msg)) {
            bslib::card(
              bslib::card_header("Column Names"),
              rhandsontable::rHandsontableOutput(ns("hot_headers"))
            )
          } else {
            bslib::card(
              bslib::card_header(
                div(
                  class = "d-flex justify-content-between align-items-center w-100",
                  "Data",
                  uiOutput(ns("row_filter_ui"))
                )
              ),
              rhandsontable::rHandsontableOutput(ns("hot"))
            )
          },
          footer = tagList(
            actionButton(
              ns("retry"),
              "Try upload again",
              class = "btn-primary"
            ),
            modalButton("Close")
          )
        )
      )
    }) |>
      bindEvent(input$open_editor)

    # Validation message shown inside the modal
    output$modal_msgs <- renderUI({
      msg <- val_log$msg
      if (nchar(trimws(msg)) == 0) {
        return(NULL)
      }
      msg <- gsub("\033\\[[0-9;]*[mGKHFABCDJK]", "", msg)
      lines <- strsplit(msg, "\n")[[1]]
      lines <- lines[nchar(trimws(lines)) > 0]
      div(HTML(paste(lines, collapse = "<br>")))
    })
    outputOptions(output, "modal_msgs", suspendWhenHidden = FALSE)

    # Column names editor (renders even when modal is closed so it's ready on open)
    output$hot_headers <- rhandsontable::renderRHandsontable({
      req(val_dat$raw_dat)

      col_names <- names(val_dat$raw_dat)
      locs <- parse_error_locations(val_log$msg)
      header_df <- setNames(
        as.data.frame(as.list(col_names), stringsAsFactors = FALSE),
        as.character(seq_along(col_names))
      )

      hot <- rhandsontable::rhandsontable(
        header_df,
        width = "100%",
        height = 75,
        rowHeaders = FALSE
      ) |>
        rhandsontable::hot_table(wordWrap = FALSE)

      for (idx in locs$col_indices) {
        if (idx >= 1 && idx <= length(col_names)) {
          hot <- hot |>
            rhandsontable::hot_col(
              idx,
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    td.style.background = '#f8d7da';
                    td.style.fontWeight = 'bold';
                    }"
            )
        }
      }
      hot
    })
    outputOptions(output, "hot_headers", suspendWhenHidden = FALSE)

    # Row filter toggle - shown in the Data card header when problem rows exist
    output$row_filter_ui <- renderUI({
      problem_rows <- parse_problem_rows(val_log$msg)
      if (length(problem_rows) == 0) {
        return(NULL)
      }
      n_total <- if (!is.null(val_dat$raw_dat)) nrow(val_dat$raw_dat) else 0

      div(
        class = "d-flex align-items-center gap-2",
        span(
          class = "badge bg-warning text-dark",
          paste(length(problem_rows), "row(s) with issues")
        ),
        checkboxInput(
          ns("show_all_rows"),
          paste0("show all ", n_total, " rows"),
          value = FALSE
        )
      )
    })
    outputOptions(output, "row_filter_ui", suspendWhenHidden = FALSE)

    # Data editor - shows only problem rows by default when they exist
    output$hot <- rhandsontable::renderRHandsontable({
      req(val_dat$raw_dat)
      dat <- val_dat$raw_dat
      problem_rows <- parse_problem_rows(val_log$msg)
      locs <- parse_error_locations(val_log$msg, names(dat))
      show_all <- isTRUE(input$show_all_rows)
      if (length(problem_rows) > 0 && !show_all) {
        valid_rows <- problem_rows[
          problem_rows >= 1 & problem_rows <= nrow(dat)
        ]
        dat <- dat[valid_rows, , drop = FALSE]
      }

      hot <- rhandsontable::rhandsontable(dat, width = "100%", height = 450) |>
        rhandsontable::hot_table(wordWrap = FALSE)

      col_names <- names(dat)
      if (length(problem_rows) > 0 || length(locs$cell_map) > 0) {
        for (i in seq_along(col_names)) {
          cn <- col_names[i]
          col_bad <- locs$cell_map[[cn]]
          cell_0 <- if (!is.null(col_bad)) {
            if (!show_all && length(problem_rows) > 0) {
              which(problem_rows %in% col_bad) - 1L
            } else {
              col_bad - 1L
            }
          } else {
            integer(0)
          }
          row_0 <- if (show_all && length(problem_rows) > 0) {
            problem_rows - 1L
          } else {
            integer(0)
          }
          if (length(row_0) == 0 && length(cell_0) == 0) {
            next
          }
          hot <- hot |>
            rhandsontable::hot_col(
              i,
              renderer = sprintf(
                "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if ([%s].indexOf(row) > -1) { td.style.background = '#fff3cd'; }
                    if ([%s].indexOf(row) > -1) { td.style.background = '#ffc107'; }
                    }",
                paste(row_0, collapse = ","),
                paste(cell_0, collapse = ",")
              )
            )
        }
      }
      hot
    })
    outputOptions(output, "hot", suspendWhenHidden = FALSE)

    # Retry ----
    observe({
      col_err <- is_column_error(val_log$msg)
      handle_retry(
        dat_name,
        val_log = val_log,
        val_dat = val_dat,
        hot_input = if (!col_err) input$hot else NULL,
        hot_headers_input = if (col_err) input$hot_headers else NULL,
        show_all = isTRUE(input$show_all_rows),
        problem_rows = parse_problem_rows(val_log$msg)
      )

      if (val_log$edit_dat != dat_name) removeModal()
    }) |>
      bindEvent(input$retry)
  })
}
