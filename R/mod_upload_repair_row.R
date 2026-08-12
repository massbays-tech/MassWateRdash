#' Repair upload errors UI
#'
#' @description `mod_upload_repair_ui()` is a helper module for
#' `mod_upload_ui()`. It lets the user edit columns and variables in an
#' interactive process.
#'
#' @param id Namespace id for module. Should match `mod_upload_repair_server()`
#' id.
#'
#' @noRd
mod_upload_repair_row_ui <- function(id) {
  ns <- NS(id)

  tagList(
    bslib::card(
      bslib::card_header(
        div(
          class = "d-flex justify-content-between align-items-center w-100",
          "Data",
          div(
            class = "d-flex align-items-center gap-2",
            span(
              class = "badge bg-warning text-dark",
              renderText(ns("problem_count"))
            ),
            checkboxInput(
              ns("show_all_rows"),
              "show all rows",
              value = FALSE
            )
          )
        )
      ),
      rhandsontable::rHandsontableOutput(ns("hot"))
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
#' @param val_dat R6 class. Dataframes.
#'
#' @noRd
mod_upload_repair_row_server <- function(id, val_dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set variables ----
    val <- reactiveValues(
      problem_rows = NULL,
      problem_dat = NULL,
      locs = NULL
    )

    observe({
      dat <- val_dat$raw_dat
      bad_rows <- parse_problem_rows(val_dat$msg)

      valid_rows <- bad_rows[
        bad_rows >= 1 & bad_rows <= nrow(dat)
      ]
      bad_dat <- dat[valid_rows, , drop = FALSE]

      val$problem_rows <- bad_rows
      val$problem_dat <- bad_dat
      val$locs <- parse_error_locations(val_dat$msg, names(val_dat$raw_dat))
    }) |>
      bindEvent(gargoyle::watch("update_val"))

    # Update UI ----
    problem_count <- textOutput({
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

    # Data editor - shows only problem rows by default when they exist
    output$hot <- rhandsontable::renderRHandsontable({
      req(val_dat$raw_dat)

      dat <- val_dat$raw_dat
      problem_rows <- val$problem_rows
      locs <- val$locs
      show_all <- input$show_all_rows

      if (length(problem_rows) > 0 && !show_all) {
        dat <- val$bad_dat
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
  })

  # Return data ----
  # MUST RETURN input$hot, input$show_all_rows
}
