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

    # Toggle edit button visibility
    output$show_btn <- renderText({
      val_visible[[dat_name]]
    }) |>
      bindEvent(gargoyle::watch("data_upload"))
    outputOptions(output, "show_btn", suspendWhenHidden = FALSE)

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
              mod_upload_repair_col_ui(ns("repair_col"))
            ),
            tabPanelBody(
              "edit_var",
              mod_upload_repair_var_ui(ns("repair_var"))
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

    # R6, gargoyle, modules ----
    val_repair <- editDat$new()
    gargoyle::init("init_repair", "update_table")
    mod_upload_repair_col_server("repair_col", val_repair)
    mod_upload_repair_var_server("repair_var", val_repair)
    mod_upload_repair_row_server("repair_row", val_repair, dat_name)

    # Update tabs, variables ----
    # When open editor, update val_dat & selected tab

    observe({
      gargoyle::watch("data_upload")
      val_repair$parse_msg(val_dat$msg, val_dat$raw_dat, dat_name)
      gargoyle::trigger("init_repair")
    }) |>
      bindEvent(input$open_editor)

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

    # Retry ----
    observe({
      gargoyle::watch("init_repair")
      gargoyle::watch("update_table")

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
