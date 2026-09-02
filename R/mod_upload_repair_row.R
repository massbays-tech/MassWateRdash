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
    reactable.extras::reactable_extras_dependency(),
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
            tags$label(
              tags$input(
                type = "checkbox",
                onclick = "Reactable.setFilter('react_rows', 'Bad_Row', event.target.checked)"
              ),
              "Show All Rows"
            )
          )
        )
      ),
      reactable::reactableOutput(ns("react_rows"))
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
#' @param val_repair R6 class.
#'
#' @noRd
mod_upload_repair_row_server <- function(id, val_dat, val_repair, dat_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update UI ----
    output$problem_count <- renderText({
      paste(length(val_repair$problem_rows), "row(s) with issues")
    }) |>
      bindEvent(gargoyle::watch("update_repair"))

    # Create table ----
    output$react_rows <- reactable::renderReactable({
      dat <- val_dat$raw_dat
      problem_rows <- val_repair$problem_rows
      locs <- val_repair$locs

      req(dat)

      dat <- dat |>
        dplyr::mutate("Delete Row" = FALSE) |>
        dplyr::relocate("Delete Row") |>
        dplyr::mutate("Bad_Row" = FALSE)

      if (length(problem_rows) > 0) {
        valid_rows <- problem_rows[
          problem_rows >= 1 & problem_rows <= nrow(dat)
        ]
        dat <- dat[valid_rows, , drop = FALSE]
        dat[valid_rows, "Bad_Row"] <- TRUE
      }

      reactable::reactable(
        dat,
        columns = list(
          "Bad_Row" = reactable::colDef(
            show = FALSE # ,
            # filterMethod = JS(
            #   "function(rows, columnId, filterValue) {
            #     if (filterValue === false) {
            #       return rows.filter(function(row) {
            #         const badRow = row.values[columnId]
            #         return badRow
            #       })
            #     }
            #     return rows
            #   }"
            # )
          )
        ),
        rowStyle = function(index) {
          if (dat[index, "Bad_Row"] == TRUE) {
            list(background = "#ffc107")
          }
        },
        elementId = "cars-missing"
      )
    }) |>
      bindEvent(gargoyle::watch("update_val"))

    # observe({
    #   session$sendCustomMessage(
    #     tableId = "react_rows",
    #     columnName = "Bad_Row",
    #     value = input$show_all_rows
    #   )
    # }) |>
    #   bindEvent(input$show_all_rows)





  })
}
