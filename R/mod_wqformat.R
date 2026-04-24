#' wqformat UI
#'
#' @description A shiny Module.
#'
#' @param id String. Namespace ID for module.
#'
#' @noRd
mod_format_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Sidebar ----
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        width = 500,
        # * Results ----
        h2("Results Data"),
        dropdown(
          ns("result_format"),
          label = "Select Results Format",
          choices = c(
            "blank", "MA_BRC", "ME_FOCB", "ME_DEP", "masswater", "RI_DEM",
            "RI_WW", "wqdashboard", "WQX", "custom"
          ),
          choice_names = c(
            " ", "Blackstone River Coalition", "Friends of Casco Bay",
            "Maine DEP", "MassWateR", "RI DEM", "URI Watershed Watch",
            "WQdashboard", "WQX", "Other"
          ),
          sorted = FALSE,
          multiple = FALSE
        ),
        conditionalPanel(
          condition = paste0(
            'output["', ns("show_result_custom"), '"] == "show"'
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
        conditionalPanel(
          condition = paste0(
            'output["', ns("show_result_download"), '"] == "show"'
          ),
          downloadButton(
            ns("result_download"),
            "Download Results (.xlsx)",
            style = "width: fit-content;"
          )
        ),
        # * Sites ----
        h2("Site Metadata"),
        dropdown(
          ns("site_format"),
          label = "Select Site Format",
          choices = c(
            "blank", "MA_BRC", "ME_FOCB", "masswater", "RI_WW", "wqdashboard",
            "WQX", "custom"
          ),
          choice_names = c(
            " ", "Blackstone River Coalition", "Friends of Casco Bay",
            "MassWateR", "URI Watershed Watch", "WQdashboard", "WQX", "Other"
          ),
          sorted = FALSE,
          multiple = FALSE
        ),
        conditionalPanel(
          condition = paste0(
            'output["', ns("show_site_custom"), '"] == "show"'
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
        conditionalPanel(
          condition = paste0(
            'output["', ns("show_site_download"), '"] == "show"'
          ),
          downloadButton(
            ns("site_download"),
            "Download Sites (.xlsx)",
            style = "width: fit-content;"
          )
        ),
      ),
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
        verbatimTextOutput(ns("validation_messages"), placeholder = FALSE)
      )
    )
  )
}

#' wqformat server
#'
#' @description A shiny module.
#'
#' @param id String. Namespace ID for module.
#'
#' @noRd
mod_format_server <- function(id) {
  moduleServer(id, function(input, output, session) {
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
    # * Results ----
    output$show_result_custom <- renderText({
      if (input$result_format == "custom") {
        "show"
      } else {
        "hide"
      }
    })
    outputOptions(output, "show_result_custom", suspendWhenHidden = FALSE)

    output$show_result_upload <- renderText({
      chk <- !input$result_format %in% c("custom", "blank")
      chk2 <- input$result_format == "custom" & !is.null(val$custom_result)

      if (chk | chk2) {
        return("show")
      } else {
        return("hide")
      }
    })
    outputOptions(output, "show_result_upload", suspendWhenHidden = FALSE)

    output$show_result_download <- renderText({
      if (!is.null(val$dat_result)) {
        return("show")
      } else {
        return("hide")
      }
    })
    outputOptions(output, "show_result_download", suspendWhenHidden = FALSE)

    # * Sites ----
    output$show_site_custom <- renderText({
      if (input$site_format == "custom") {
        "show"
      } else {
        "hide"
      }
    })
    outputOptions(output, "show_site_custom", suspendWhenHidden = FALSE)

    output$show_site_upload <- renderText({
      chk <- !input$site_format %in% c("custom", "blank")
      chk2 <- input$site_format == "custom" & !is.null(val$custom_site)

      if (chk | chk2) {
        return("show")
      } else {
        return("hide")
      }
    })
    outputOptions(output, "show_site_upload", suspendWhenHidden = FALSE)

    output$show_site_download <- renderText({
      if (!is.null(val$dat_site)) {
        return("show")
      } else {
        return("hide")
      }
    })
    outputOptions(output, "show_site_download", suspendWhenHidden = FALSE)

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
      req(input$result_format)
      req(input$result_upload)

      val$message_log <- "Uploading result data..."
      val$error_log <- ""

      dat <- tryCatch(
        {
          capture_local_messages(
            upload_custom_results(
              input$result_upload,
              input$result_format,
              val$custom_result
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
      req(input$site_format)
      req(input$site_upload)

      val$message_log <- "Uploading site metadata..."
      val$error_log <- ""

      dat <- tryCatch(
        {
          capture_local_messages(
            upload_custom_sites(
              input$site_upload,
              input$site_format,
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

    # Download data ----
    output$result_download <- downloadHandler(
      filename = function() {
        "masswater_results.xlsx"
      },
      content = function(file) {
        writexl::write_xlsx(val$dat_result, path = file)
      }
    )

    output$site_download <- downloadHandler(
      filename = function() {
        "masswater_sites.xlsx"
      },
      content = function(file) {
        writexl::write_xlsx(val$dat_site, path = file)
      }
    )

    # UI messages ----
    output$result_status <- renderUI({
      fl_status(
        tester = FALSE,
        file_input = input$result_upload,
        data_state = val$dat_result
      )
    })

    output$custom_result_status <- renderUI({
      if (input$result_format == "custom") {
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
      if (input$site_format == "custom") {
        fl_status(
          tester = FALSE,
          file_input = input$site_custom,
          data_state = val$custom_site
        )
      } else {
        HTML("<span style='color:#9c9c9c'>N/A</span>")
      }
    })

    output$validation_messages <- renderText({
      val$message_log
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
