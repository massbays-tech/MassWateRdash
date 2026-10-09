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
    shinyjs::useShinyjs(),
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
            checkboxInput(ns("show_all"), label = "Show All Rows")
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
      bindEvent(gargoyle::watch("init_repair"))

    # Set variables ----
    filter_rows <- reactive({
      bad_rows <- val_repair$problem_rows
      if (isTruthy(input$show_all) | length(bad_rows) == 0) {
        FALSE
      } else {
        TRUE
      }
    }) |>
      bindEvent(gargoyle::watch("init_repair"), input$show_all)

    dat_filter <- reactive({
      gargoyle::watch("update_table")

      if (!filter_rows()) {
        return (val_repair$df_row)
      }

      dplyr::filter(val_repair$df_row, .data$bad_row == TRUE)
    }) |>
      bindEvent(gargoyle::watch("init_repair"), filter_rows())

    react_style <- reactive({
      dat <- val_repair$df_row
      req(dat)

      special_col <- c(
        "ID", "bad_row", "Activity Type", "Activity Depth/Height Unit",
        "Activity Relative Depth Name", "Characteristic Name", "Result Unit",
        "Parameter", "uom"
      )
      col_list <- setdiff(colnames(dat), special_col)

      col_def <- list(
        "ID" = reactable::colDef(name = "Row"),
        "bad_row" = reactable::colDef(show = FALSE)
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
            unique(c(" ", mwr_activity, dat$`Activity Type`)),
            class = "dropdown-extra"
          )
        )
        col_def[["Activity Depth/Height Unit"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_depth_unit"),
            unique(c(" ", "ft", "m", dat$`Activity Depth/Height Unit`)),
            class = "dropdown-extra"
          )
        )
        col_def[["Activity Relative Depth Name"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_depth"),
            unique(
              c(" ", "Surface", "Midwater", "Near Bottom", "Bottom",
                dat$`Activity Relative Depth Name`)
            ),
            class = "dropdown-extra"
          )
        )
        col_def[["Characteristic Name"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_param"),
            unique(c(" ", mwr_param, dat$`Characteristic Name`)),
            class = "dropdown-extra"
          )
        )
        col_def[["Result Unit"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_unit"),
            unique(c(" ", mwr_unit, dat$`Result Unit`)),
            class = "dropdown-extra"
          )
        )
      } else if (dat_name == "accdat") {
        col_def[["Parameter"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_param"),
            unique(c(" ", mwr_param, dat$Parameter)),
            class = "dropdown-extra"
          )
        )
        col_def[["uom"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_unit"),
            unique(c(" ", mwr_unit, dat$uom)),
            class = "dropdown-extra"
          )
        )
      } else if (dat_name %in% c("frecomdat", "wqxdat", "censdat")) {
        col_def[["Parameter"]] <- reactable::colDef(
          cell = reactable.extras::dropdown_extra(
            ns("var_param"),
            unique(c(" ", mwr_param, dat$Parameter)),
            class = "dropdown-extra"
          )
        )
      }

      col_def
    }) |>
      bindEvent(gargoyle::watch("init_repair"))

    # Create, update table ----
    output$react_rows <- reactable::renderReactable({
      # Reset cached values
      # https://github.com/Appsilon/reactable.extras/issues/50
      shinyjs::runjs("memory = {};")

      reactable::reactable(
        dat_filter(),
        columns = react_style(),
        rowStyle = htmlwidgets::JS(
          "function(rowInfo) {
              if (rowInfo.values['bad_row'] == true) {
                return { backgroundColor: '#f9cfb4' }
              }
            }"
        )
      )
    }) |>
      bindEvent(dat_filter(), react_style())

    # Update dataframe ----
    observe({
      gargoyle::watch("update_table")
      val_repair$edit_row(input$var_text, filter_rows())
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_text)

    observe({
      gargoyle::watch("update_table")
      val_repair$edit_row(input$var_activity, filter_rows())
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_activity)

    observe({
      gargoyle::watch("update_table")
      val_repair$edit_row(input$var_param, filter_rows())
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_param)

    observe({
      gargoyle::watch("update_table")
      val_repair$edit_row(input$var_unit, filter_rows())
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_unit)

    observe({
      gargoyle::watch("update_table")
      val_repair$edit_row(input$var_depth, filter_rows())
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_depth)

    observe({
      gargoyle::watch("update_table")
      val_repair$edit_row(input$var_depth_unit, filter_rows())
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$var_depth_unit)

  })
}
