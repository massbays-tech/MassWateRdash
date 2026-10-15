#' wqformat UI
#'
#' @description A shiny Module.
#'
#' @param id String. Namespace ID for module.
#'
#' @noRd
mod_upload_format_ui <- function(id, in_modal = FALSE) {
  ns <- NS(id)

  fmt_sidebar <- bslib::sidebar(
    width = 350,
    # * Format ----
    h2("Input Format"),
    dropdown(
      ns("in_format"),
      label = "Select Input Format",
      choices = c(
        "masswater", "ma_brc", "me_focb", "me_dep", "ri_dem",
        "ri_ww", "wqdashboard", "wqx", "custom"
      ),
      choice_names = c(
        "MassWateR", "Blackstone River Coalition", "Friends of Casco Bay",
        "Maine DEP", "RI DEM", "Watershed Watch (URI)",
        "WQdashboard", "WQX", "Other"
      ),
      sorted = FALSE,
      multiple = FALSE
    ),
    dropdown(
      ns("date_format"),
      label = "Select Date Format",
      choices = c("Y/m/d", "y/m/d", "m/d/Y", "m/d/y", "d/m/Y", "d/m/y"),
      choice_names = c(
        "yyyy/mm/dd", "yy/mm/dd", "mm/dd/yyyy", "mm/dd/yy", "dd/mm/yyyy",
        "dd/mm/yy"
      ),
      sorted = FALSE,
      multiple = FALSE
    ),
    checkboxInput(ns("is_datetime"), "Date column includes time"),
    # * Results ----
    h2("Results Data"),
    conditionalPanel(
      condition = paste0(
        'output["', ns("show_custom"), '"] == "show"'
      ),
      fileInput(
        ns("result_custom"),
        "Upload Custom Result Format (.xlsx)",
        accept = ".xlsx"
      )
    ),
    conditionalPanel(
      condition = paste0(
        'output["', ns("show_result_upload"), '"] == "show"'
      ),
      fileInput(
        ns("result_upload"),
        "Upload Results Data (.xlsx)",
        accept = ".xlsx"
      )
    ),
    # * Sites ----
    h2("Site Metadata"),
    conditionalPanel(
      condition = paste0(
        'output["', ns("show_custom"), '"] == "show"'
      ),
      fileInput(
        ns("site_custom"),
        "Upload Custom Site Format (.xlsx)",
        accept = ".xlsx"
      )
    ),
    conditionalPanel(
      condition = paste0(
        'output["', ns("show_site_upload"), '"] == "show"'
      ),
      fileInput(
        ns("site_upload"),
        "Upload Site Metadata (.xlsx)",
        accept = ".xlsx"
      )
    ),
  )

  fmt_main <- tagList(
    # Upload status ----
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = "Results Data",
        value = htmlOutput(ns("result_status"))
      ),
      bslib::value_box(
        title = "Custom Result Format",
        value = htmlOutput(ns("custom_result_status"))
      ),
      bslib::value_box(
        title = "Site Metadata",
        value = htmlOutput(ns("site_status"))
      ),
      bslib::value_box(
        title = "Custom Site Format",
        value = htmlOutput(ns("custom_site_status"))
      )
    ),
    # Validation text ----
    bslib::card(
      bslib::card_header("Data Validation Messages"),
      uiOutput(ns("validation_messages"), placeholder = FALSE)
    )
  )

  if (in_modal) {
    bslib::layout_sidebar(sidebar = fmt_sidebar, fmt_main)
  } else {
    tagList(bslib::page_sidebar(sidebar = fmt_sidebar, fmt_main))
  }
}

#' wqformat server
#'
#' @description A shiny module.
#'
#' @param id String. Namespace ID for module.
#'
#' @noRd
mod_upload_format_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set variables ----
    val <- reactiveValues(
      message_log = "",
      error_log = "",
      custom_result = NULL,
      custom_site = NULL,
      dat_result = NULL,
      dat_site = NULL
    )

    # Toggle UI ----
    output$show_custom <- renderText({
      req(input$in_format)

      if (input$in_format == "custom") {
        "show"
      } else {
        "hide"
      }
    })
    outputOptions(output, "show_custom", suspendWhenHidden = FALSE)

    observe({
      req(input$in_format)

      if (input$in_format == "ma_brc") {
        updateCheckboxInput(session, "is_datetime", value = TRUE)
      } else if (input$in_format != "custom") {
        updateCheckboxInput(session, "is_datetime", value = FALSE)
      }
    }) |>
      bindEvent(input$in_format)

    output$show_result_upload <- renderText({
      req(input$in_format)
      chk <- input$in_format == "custom" & is.null(val$custom_result)

      if (chk) {
        return("hide")
      } else {
        return("show")
      }
    })
    outputOptions(output, "show_result_upload", suspendWhenHidden = FALSE)

    output$show_site_upload <- renderText({
      req(input$in_format)
      chk <- input$in_format == "custom" & is.null(val$custom_site)

      if (chk) {
        return("hide")
      } else {
        return("show")
      }
    })
    outputOptions(output, "show_site_upload", suspendWhenHidden = FALSE)

    # Upload data -----
    # * Result Format ----
    observe({
      req(input$result_custom)

      msg <- "Uploading custom result format..."
      val$message_log <- msg
      val$error_log <- ""

      dat <- tryCatch(
        {
          capture_local_messages(
            upload_result_format(input$result_custom)
          )
        },
        error = function(e) {
          val$error_log <- paste("Error:", e$message)
          NULL
        }
      )

      if (nchar(val$error_log) > 0) {
        val$custom_result <- NULL
        val$message_log <- val$error_log
      } else {
        val$custom_result <- dat$dat
        val$message_log <- dat$msg
      }
    }) |>
      bindEvent(input$result_custom)

    # * Result Data ----
    observe({
      req(input$in_format)
      req(input$result_upload)

      val$message_log <- "Uploading result data..."
      val$error_log <- ""

      date_format <- if (isTruthy(input$is_datetime)) {
        paste(input$date_format, "H:M")
      } else {
        input$date_format
      }

      dat <- tryCatch(
        {
          capture_local_messages(
            upload_custom_results(
              input$result_upload,
              input$in_format,
              custom_format = val$custom_result,
              date_format = date_format
            )
          )
        },
        error = function(e) {
          val$error_log <- paste("Error:", e$message)
          NULL
        }
      )

      if (nchar(val$error_log) > 0) {
        val$dat_result <- NULL
        val$message_log <- val$error_log
      } else {
        val$dat_result <- dat$dat
        val$message_log <- dat$msg
      }
    }) |>
      bindEvent(input$result_upload)

    # * Site format ----
    observe({
      req(input$site_custom)

      msg <- "Uploading custom site format..."
      val$message_log <- msg
      val$error_log <- ""

      dat <- tryCatch(
        {
          capture_local_messages(
            upload_site_format(input$site_custom)
          )
        },
        error = function(e) {
          val$error_log <- paste("Error:", e$message)
          NULL
        }
      )

      if (nchar(val$error_log) > 0) {
        val$custom_site <- NULL
        val$message_log <- val$error_log
      } else {
        val$custom_site <- dat$dat
        val$message_log <- paste(dat$msg)
      }
    }) |>
      bindEvent(input$site_custom)


    # * Site Metadata ----
    observe({
      req(input$in_format)
      req(input$site_upload)

      val$message_log <- "Uploading site metadata..."
      val$error_log <- ""

      dat <- tryCatch(
        {
          capture_local_messages(
            upload_custom_sites(
              input$site_upload,
              input$in_format,
              val$custom_site
            )
          )
        },
        error = function(e) {
          val$error_log <- paste("Error:", e$message)
          NULL
        }
      )

      if (nchar(val$error_log) > 0) {
        val$dat_site <- NULL
        val$message_log <- val$error_log
      } else {
        val$dat_site <- dat$dat
        val$message_log <- dat$msg
      }
    }) |>
      bindEvent(input$site_upload)

    # UI messages ----
    output$result_status <- renderUI({
      fl_status(
        tester = FALSE,
        file_input = input$result_upload,
        data_state = val$dat_result
      )
    })

    output$custom_result_status <- renderUI({
      if (input$in_format == "custom") {
        fl_status(
          tester = FALSE,
          file_input = input$result_custom,
          data_state = val$custom_result
        )
      } else {
        HTML("<span style='color:#9c9c9c'>N/A</span>")
      }
    })

    output$site_status <- renderUI({
      fl_status(
        tester = FALSE,
        file_input = input$site_upload,
        data_state = val$dat_site
      )
    })

    output$custom_site_status <- renderUI({
      if (input$in_format == "custom") {
        fl_status(
          tester = FALSE,
          file_input = input$site_custom,
          data_state = val$custom_site
        )
      } else {
        HTML("<span style='color:#9c9c9c'>N/A</span>")
      }
    })

    output$validation_messages <- renderUI({
      format_log(val$message_log)
    })

    # Return data ----
    return(
      list(
        dat_results = reactive({
          val$dat_result
        }),
        dat_sites = reactive({
          val$dat_site
        })
      )
    )
  })
}
