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
    reactable.extras::reactable_extras_dependency(),
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
#' @param val_dat R6 class. Includes data specific to `dat_name`. Contains the
#' variables `dat`, `raw_dat`, and `msg`.  `dat` and `raw_dat` are dataframes;
#' `msg` is a string.
#' @param val_visible R6 class. Controls UI and modal visibility.
#'
#' @noRd
mod_upload_repair_server <- function(id, dat_name, val_log, val_visible, val_dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show/hide edit button -----
    output$show_btn <- renderText({
      val_visible[[dat_name]]
    }) |>
      bindEvent(gargoyle::watch("data_upload"))
    outputOptions(output, "show_btn", suspendWhenHidden = FALSE)

    # R6, gargoyle, modules ----
    val_repair <- editDat$new()
    gargoyle::init("init_repair", "update_col", "update_var", "update_row")
    mod_upload_repair_row_server("repair_row", val_repair, dat_name)

    # Create modal ----
    observe({
      gargoyle::watch("data_upload")
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
              reactable.extras::reactable_extras_dependency(),
              bslib::card(
                bslib::card_header("Column Names"),
                reactable::reactableOutput(ns("col_table"))
              )
            ),
            tabPanelBody(
              "edit_var",
              bslib::card(
                bslib::card_header("Invalid Variables"),
                reactable::reactableOutput(ns("var_table"))
              )
            ),
            tabPanelBody(
              "edit_row",
              mod_upload_repair_row_ui(ns("repair_row"))
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

    # Update tabs, variables ----
    # When open editor or upload data, update val_dat & selected tab
    observe({
      val_repair$parse_msg(val_dat$msg, val_dat$raw_dat, dat_name)

      gargoyle::trigger("init_repair")
      gargoyle::trigger("update_col")
      gargoyle::trigger("update_var")
      gargoyle::trigger("update_row")
    }) |>
      bindEvent(input$open_editor, gargoyle::watch("data_upload"))

    observe({
      tab <- if (!is.null(val_repair$df_col)) {
        "edit_col"
      } else if (!is.null(val_repair$df_var)) {
        "edit_var"
      } else {
        "edit_row"
      }

      updateTabsetPanel(inputId = "tabset", selected = tab)
    }) |>
      bindEvent(gargoyle::watch("init_repair"))

    # Validation message ----
    output$modal_msgs <- renderUI({
      format_log(val_dat$msg)
    }) |>
      bindEvent(gargoyle::watch("data_upload"))

    # Edit columns ----
    output$col_table <- reactable::renderReactable({
      if (is.null(val_repair$df_col)) {
        return(NULL)
      }

      var_list <- c(" ", val_repair$missing_col)

      reactable::reactable(
        val_repair$df_col,
        columns = list(
          "Delete Column" = reactable::colDef(
            cell = reactable.extras::checkbox_extra(
              ns("del_col"), class = "checkbox-extra"
            ),
            align = "left"
          ),
          "New Column Name" = reactable::colDef(
            cell = reactable.extras::dropdown_extra(
              ns("new_col"), var_list, class = "dropdown-extra"
            )
          )
        )
      )
    }) |>
      bindEvent(gargoyle::watch("init_repair"))

    observe({
      gargoyle::watch("update_col")

      val <- input$new_col
      val_repair$df_col[val$row, "New Column Name"] <- val$value

      gargoyle::trigger("update_col")
    }) |>
      bindEvent(input$new_col)

    observe({
      gargoyle::watch("update_col")

      val <- input$del_col
      val_repair$df_col[val$row, "Delete Column"] <- val$value

      gargoyle::trigger("update_col")
    }) |>
      bindEvent(input$del_col)

    # Mass edit variables ----
    output$var_table <- reactable::renderReactable({
      if (is.null(val_repair$df_var)) {
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
        val_repair$df_var,
        columns = list(
          "Replace With" = reactable::colDef(
            cell = reactable.extras::dropdown_extra(
              ns("new_var"), var_list, class = "dropdown-extra"
            )
          )
        )
      )
    }) |>
      bindEvent(gargoyle::watch("init_repair"))

    observe({
      gargoyle::watch("update_var")

      val <- input$new_var
      val_repair$df_var[val$row, "Replace With"] <- val$value

      gargoyle::trigger("update_var")
    }) |>
      bindEvent(input$new_var)

    # Retry ----
    observe({
      gargoyle::watch("update_col")
      gargoyle::watch("update_var")
      gargoyle::watch("update_row")

      if (input$tabset == "edit_col") {
        val_repair$df_col |>
          update_hot_col(val_dat$raw_dat) |>
          handle_retry(dat_name, val_log, val_visible, val_dat)
      } else if (input$tabset == "edit_var") {
        edited_df <- update_hot_var(val_repair$df_var, val_dat$raw_dat)

        if (is.null(edited_df)) {
          updateTabsetPanel(inputId = "tabset", selected = "edit_row")
        } else {
          handle_retry(edited_df, dat_name, val_log, val_visible, val_dat)
        }
      } else {
        update_hot_rows(val_repair$df_row) |>
          handle_retry(dat_name, val_log, val_visible, val_dat)
        val_repair$df_row <- val_repair$df_row |>
          dplyr::mutate("ID" = dplyr::row_number())
      }

      gargoyle::trigger("data_upload")
      if (!val_visible[[dat_name]]) removeModal()
    }) |>
      bindEvent(input$retry)
  })
}
