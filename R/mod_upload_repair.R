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
          tabsetPanel(
            id = ns("tabset"),
            type = "hidden",
            tabPanelBody(
              "edit_col",
              bslib::card(
                bslib::card_header("Column Names"),
                rhandsontable::rHandsontableOutput(ns("hot_headers"))
              )
            ),
            tabPanelBody(
              "edit_var",
              bslib::card(
                bslib::card_header("Invalid Variables"),
                rhandsontable::rHandsontableOutput(ns("hot_var"))
              )
            ),
            tabPanelBody(
              "edit_row",
              bslib::card(
                bslib::card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center w-100",
                    "Data",
                    div(
                      class = "d-flex align-items-center gap-2",
                      span(
                        class = "badge bg-warning text-dark",
                        textOutput(ns("problem_count"))
                      ),
                      checkboxInput(
                        ns("show_all_rows"),
                        "show all rows",
                        value = FALSE
                      )
                    )
                  )
                ),
                rhandsontable::rHandsontableOutput(ns("hot_rows"))
              )
            )
          ),
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

    # Set variables ----
    val <- reactiveValues(
      problem_rows = NULL,
      repeat_errors = NULL,
      locs = NULL
    )

    # Update tabs, reactive variables ----
    observe({
      if (is_column_error(val_dat$msg)) {
        print("setting tab to edit_col")
        updateTabsetPanel(inputId = "tabset", selected = "edit_col")
      } else {
        val$problem_rows <- parse_problem_rows(val_dat$msg)
        val$locs <- parse_error_locations(val_dat$msg, names(val_dat$raw_dat))
        val$repeat_errors <- parse_repeat_errors(val_dat$raw_dat, val$locs)

        if (is.null(val$repeat_errors)) {
          print("setting tab to edit_row")
          updateTabsetPanel(inputId = "tabset", selected = "edit_row")
        } else {
          print("setting tab to edit_var")
          updateTabsetPanel(inputId = "tabset", selected = "edit_var")
        }
      }
    }) |>
      bindEvent(gargoyle::watch("update_val"), input$open_editor)

    # Update UI ----
    output$problem_count <- renderText({
      paste(length(val$problem_rows), "row(s) with issues")
    })

    observe({
      n_total <- if (!is.null(val_dat$raw_dat)) nrow(val_dat$raw_dat) else 0

      updateCheckboxInput(
        session = session,
        inputId = "show_all_rows",
        label = paste("show all", n_total, "rows")
      )
    }) |>
      bindEvent(gargoyle::watch("update_val"))

    # Validation message ----
    output$modal_msgs <- renderUI({
      format_log(val_dat$msg)
    }) |>
      bindEvent(gargoyle::watch("update_val"))

    # Edit columns ----
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

    # Edit Variables ----
    output$hot_var <- rhandsontable::renderRHandsontable({
      val$repeat_errors
    })

    # Edit Rows ----
    output$hot_rows <- rhandsontable::renderRHandsontable({
      req(val_dat$raw_dat)

      dat <- val_dat$raw_dat
      problem_rows <- val$problem_rows
      locs <- val$locs
      show_all <- input$show_all_rows

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

    # Retry ----
    observe({
      gargoyle::watch("update_val")

      if (input$tabset == "edit_col") {
        update_hot_col(val_dat, input$hot_headers) |>
          handle_retry(dat_name, val_log, val_edit, val_dat)
        gargoyle::trigger("update_val")
      } else if (input$tabset == "edit_var") {
        edited_df <- update_hot_var(val_dat, input$hot_var)

        if (is.null(edited_df)) {
          updateTabsetPanel(inputId = "tabset", selected = "edit_row")
        } else {
          handle_retry(edited_df, dat_name, val_log, val_edit, val_dat)
          gargoyle::trigger("update_val")
        }
      } else {
        update_hot_row(
          val_dat = val_dat,
          hot_table = input$hot_rows,
          show_all = input$show_all_rows,
          problem_rows = val$problem_rows
        ) |>
          handle_retry(dat_name, val_log, val_edit, val_dat)
        gargoyle::trigger("update_val")
      }

      if (!val_edit[[dat_name]]) removeModal()
    }) |>
      bindEvent(input$retry)
  })
}
