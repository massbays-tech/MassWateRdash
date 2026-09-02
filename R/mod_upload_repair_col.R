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
mod_upload_repair_col_ui <- function(id) {
  ns <- NS(id)

  tagList(
    reactable.extras::reactable_extras_dependency(),
    bslib::card(
      bslib::card_header("Column Names"),
      reactable::reactableOutput(ns("col_table"))
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
mod_upload_repair_col_server <- function(id, val_repair) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Edit Variables ----
    output$col_table <- reactable::renderReactable({
      if (is.null(val_repair$problem_col)) {
        return(NULL)
      }

      var_list <- c(" ", val_repair$missing_col)

      reactable::reactable(
        val_repair$problem_col,
        columns = list(
          "Delete Column" = reactable::colDef(
            cell = reactable.extras::checkbox_extra(
              ns("check"), class = "checkbox-extra"
            ),
            align = "left"
          ),
          "New Column Name" = reactable::colDef(
            cell = reactable.extras::dropdown_extra(
              ns("dropdown"), var_list, class = "dropdown-extra"
            )
          )
        )
      )
    }) |>
      bindEvent(gargoyle::watch("update_repair"))

    observe({
      gargoyle::watch("update_repair")
      gargoyle::watch("update_table")

      val_repair$df_col[input$dropdown$row, "New Column Name"] <- input$dropdown$value
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$dropdown)

    observe({
      gargoyle::watch("update_repair")
      gargoyle::watch("update_table")

      val_repair$df_col[input$check$row, "Delete Column"] <- input$check$value
      gargoyle::trigger("update_table")
    }) |>
      bindEvent(input$check)
  })
}
