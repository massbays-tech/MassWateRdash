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
      condition = paste0('output["', ns("show_btn"), '"] == "TRUE"'),
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
#' @param val_edit R6 class. Controls UI and modal visibility.
#' @param val_dat R6 class. Dataframes.
#'
#' @noRd
mod_upload_repair_server <- function(id, dat_name, val_log, val_edit, val_dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Toggle edit button visibility
    output$show_btn <- renderText({
      val_edit[[dat_name]]
    }) |>
      bindEvent(gargoyle::watch("update_val"))
    outputOptions(output, "show_btn", suspendWhenHidden = FALSE)

    # Modules ----
    repair_row <- mod_upload_repair_row_server("repair_row", val_dat)

    # Create modal ----
    observe({
      gargoyle::watch("update_val")
      showModal(
        modalDialog(
          title = paste(
            "Edit", unname(file_labels[dat_name]), "- Fix Validation Errors"
          ),
          size = "xl",
          easyClose = FALSE,
          uiOutput(ns("modal_msgs")),
          br(),
          p("Fix the issue below, then click 'Try upload again'."),
          if (is_column_error(val_dat$msg)) {
            bslib::card(
              bslib::card_header("Column Names"),
              rhandsontable::rHandsontableOutput(ns("hot_headers"))
            )
          } else {
            mod_upload_repair_row_ui(ns("repair_row"))
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
      format_log(val_dat$msg)
    }) |>
      bindEvent(gargoyle::watch("update_val"))
    outputOptions(output, "modal_msgs", suspendWhenHidden = FALSE)

    # Column names editor (renders even when modal is closed so it's ready on open)
    output$hot_headers <- rhandsontable::renderRHandsontable({
      req(val_dat$raw_dat)

      col_names <- names(val_dat$raw_dat)
      locs <- parse_error_locations(val_dat$msg)
      header_df <- setNames(
        as.data.frame(as.list(col_names), stringsAsFactors = FALSE),
        as.character(seq_along(col_names))
      )

      hot <- rhandsontable::rhandsontable(
        header_df,
        width = "100%", height = 75, rowHeaders = FALSE
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
    }) |>
      bindEvent(gargoyle::watch("update_val"))
    outputOptions(output, "hot_headers", suspendWhenHidden = FALSE)

    # Retry ----
    observe({
      gargoyle::watch("update_val")
      col_err <- is_column_error(val_dat$msg)
      handle_retry(
        dat_name,
        val_log = val_log,
        val_edit = val_edit,
        val_dat = val_dat,
        hot_input = if (!col_err) input$hot else NULL,
        hot_headers_input = if (col_err) input$hot_headers else NULL,
        show_all = isTRUE(input$show_all_rows),
        problem_rows = parse_problem_rows(val_dat$msg)
      )
      gargoyle::trigger("update_val")
      if (!val_edit[[dat_name]]) removeModal()
    }) |>
      bindEvent(input$retry)
  })
}
