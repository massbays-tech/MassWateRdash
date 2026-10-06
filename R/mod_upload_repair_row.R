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
            ) # ,
            # tags$label(
            #   tags$input(
            #     type = "checkbox",
            #     onclick = "Reactable.setFilter('react-rows', 'Bad_Row', event.target.checked)"
            #   ),
            #   "Show All Rows"
            # )
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
mod_upload_repair_row_server <- function(id, val_repair, dat_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update UI ----
    output$problem_count <- renderText({
      paste(length(val_repair$problem_rows), "row(s) with issues")
    }) |>
      bindEvent(gargoyle::watch("update_repair"))

    # Create table ----
    output$react_rows <- reactable::renderReactable({
      dat <- val_repair$df_row
      problem_rows <- val_repair$problem_rows
      locs <- val_repair$locs

      req(dat)

      col_list <- colnames(dat)

      dat <- dat |>
        dplyr::mutate("Bad_Row" = FALSE)
      dat[problem_rows, "Bad_Row"] <- TRUE

      if (length(problem_rows) > 0) {
        valid_rows <- problem_rows[
          problem_rows >= 1 & problem_rows <= nrow(dat)
        ]
        dat <- dat[valid_rows, , drop = FALSE]
        dat[valid_rows, "Bad_Row"] <- TRUE
      }

      special_col <- c(
        "Activity Type", "Activity Depth/Height Unit",
        "Activity Relative Depth Name", "Characteristic Name", "Result Unit",
        "Parameter", "uom"
      )
      col_list <- setdiff(col_list, special_col)

      col_def <- list(
        "Bad_Row" = reactable::colDef(
          show = FALSE # ,
          # filterMethod = reactable::JS(
          #   "function(rows, columnId, filterValue) {
          #       if (filterValue === false) {
          #         return rows.filter(function(row) {
          #           const badRow = row.values[columnId]
          #           return badRow
          #         })
          #       }
          #       return rows
          #     }"
          # )
        )
      )

      for (i in col_list) {
        col_def[[i]] <- reactable::colDef(
          cell = reactable.extras::text_extra(ns("var_text"))
        )
      }

      if (dat_name == "resdat") {
        col_def[["Activity Type"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_activity"),
            unique(c(mwr_activity, dat$`Activity Type`)),
            class = "dropdown-extra"
          )
        )
        col_def[["Activity Depth/Height Unit"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_depth_unit"),
            unique(c("ft", "m", dat$`Activity Depth/Height Unit`)),
            class = "dropdown-extra"
          )
        )
        col_def[["Activity Relative Depth Name"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_depth"),
            unique(
              c("Surface", "Midwater", "Near Bottom", "Bottom",
                dat$`Activity Relative Depth Name`)
            ),
            class = "dropdown-extra"
          )
        )
        col_def[["Characteristic Name"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_param"),
            unique(c(mwr_param, dat$`Characteristic Name`)),
            class = "dropdown-extra"
          )
        )
        col_def[["Result Unit"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_unit"),
            unique(c(mwr_unit, dat$`Result Unit`)),
            class = "dropdown-extra"
          )
        )
      } else if (dat_name == "accdat") {
        col_def[["Parameter"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_param"),
            unique(c(mwr_param, dat$Parameter)),
            class = "dropdown-extra"
          )
        )
        col_def[["uom"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_unit"),
            unique(c(mwr_unit, dat$uom)),
            class = "dropdown-extra"
          )
        )
      } else if (dat_name %in% c("frecomdat", "wqxdat", "censdat")) {
        col_def[["Parameter"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_param"),
            unique(c(mwr_param, dat$Parameter)),
            class = "dropdown-extra"
          )
        )
      }

      reactable::reactable(
        dat,
        columns = col_def,
        rowStyle = function(index) {
          if (dat[index, "Bad_Row"] == TRUE) {
            list(background = "#ffc107")
          }
        },
        elementId = "react-rows"
      )
    }) |>
      bindEvent(gargoyle::watch("update_val"))

    # observe({
    #   session$sendCustomMessage(
    #     tableId = "react-rows",
    #     columnName = "Bad_Row",
    #     value = input$show_all_rows
    #   )
    # }) |>
    #   bindEvent(input$show_all_rows)

    # Update table ----
    observe({
      gargoyle::watch("update_repair")
      gargoyle::watch("update_table")

      val <- input$var_text

      val_repair$df_row[val$row, val$column] <- val$value
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_text)

    observe({
      gargoyle::watch("update_repair")
      gargoyle::watch("update_table")

      val <- input$var_param

      val_repair$df_row[val$row, val$column] <- val$value
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_param)

    observe({
      gargoyle::watch("update_repair")
      gargoyle::watch("update_table")

      val <- input$var_unit

      val_repair$df_row[val$row, val$column] <- val$value
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_unit)

    observe({
      gargoyle::watch("update_repair")
      gargoyle::watch("update_table")

      val <- input$var_depth

      val_repair$df_row[val$row, val$column] <- val$value
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_depth)

    observe({
      gargoyle::watch("update_repair")
      gargoyle::watch("update_table")

      val <- input$var_depth_unit

      val_repair$df_row[val$row, val$column] <- val$value
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_depth_unit)

    observe({
      gargoyle::watch("update_repair")
      gargoyle::watch("update_table")

      val <- input$var_activity

      val_repair$df_row[val$row, val$column] <- val$value
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_activity)

  })
}
