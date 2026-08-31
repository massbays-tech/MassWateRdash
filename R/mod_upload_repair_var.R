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
mod_upload_repair_var_ui <- function(id) {
  ns <- NS(id)

  tagList(
    reactable.extras::reactable_extras_dependency(),
    bslib::card(
      bslib::card_header("Invalid Variables"),
      reactable::reactableOutput(ns("var_table"))
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
mod_upload_repair_var_server <- function(id, val_repair) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Edit Variables ----
    output$var_table <- reactable::renderReactable({
      if (is.null(val_repair$repeat_errors)) {
        return(NULL)
      }

      target_col <- names(val_repair$locs$cell_map)[1]

      var_list <- if (target_col == "Activity Type") {
        c(" ", mwr_activity)
      } else if (target_col == "Activity Depth/Height Unit") {
        c(" ", "ft", "m")
      } else if (target_col %in% c("Parameter", "Characteristic Name")) {
        c(" ", mwr_param)
      } else {
        c(" ", mwr_unit)
      }

      reactable::reactable(
        val_repair$repeat_errors,
        columns = list(
          "Delete Rows" = reactable::colDef(
            cell = reactable.extras::checkbox_extra(
              ns("check"), class = "checkbox-extra"
            ),
            align = "left"
          ),
          "Replace With" = reactable::colDef(
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
      gargoyle::watch("update_df_var")

      val_repair$df_var[input$dropdown$row, "Replace With"] <- input$dropdown$value
      gargoyle::trigger("update_df_var")
    }) |>
      bindEvent(input$dropdown)

    observe({
      gargoyle::watch("update_repair")
      gargoyle::watch("update_df_var")

      val_repair$df_var[input$check$row, "Delete Rows"] <- input$check$value
      gargoyle::trigger("update_df_var")
    }) |>
      bindEvent(input$check)
  })
}
