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
            "Upload Conversion Table (.xlsx)",
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
            "Upload Conversion Table (.xlsx)",
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
          title = "Conversion Table",
          value = htmlOutput(ns("custom_result_status"))
        ),
        bslib::value_box(
          title = "Site Metadata",
          value = htmlOutput(ns("site_status"))
        ),
        bslib::value_box(
          title = "Conversion Table",
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
      message_log = NULL,
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
    # * Results -----
    observe({
      req(input$result_custom)

      in_dat <- input$result_custom$datapath

      col_result <- custom_format(in_dat, "Column Names")
      var_parameter <- custom_format(in_dat, "Parameters")
      var_unit <- custom_format(in_dat, "Units")
      var_qualifier <- custom_format(in_dat, "Qualifiers")
      var_activity <- custom_format(in_dat, "Activity Types")

      # Check - allow custom result format?
      all_val <- c(
        val$col_result, val$var_activity, val$var_parameter, val$var_unit,
        val$var_qualifier
      )

      if (is.null(all_val)) {
        val$custom_result <- NULL
      } else {
        val$custom_result <- list(
          col_name = col_result,
          param = var_parameter,
          param_unit = var_unit,
          qualifier = var_qualifier,
          activity = var_activity
        )
      }
    }) |>
      bindEvent(input$result_custom)

    observe({
      req(input$result_format)
      req(input$result_upload)

      val$dat_result <- NULL

      # Read in file
      message("Reading file")
      dat <- readxl::read_excel(
        input$in_results,
        na = c("NA", "na", ""),
        guess_max = Inf
      ) |>
        dplyr::mutate_if(function(x) !lubridate::is.POSIXct(x), as.character)

      if (input$result_format == "masswater") {
        dat <- wqformat::format_mwr_results(dat)
      } else if (input$result_format == "custom") {
        dat <- format_custom_results(dat, val$custom_result)
      } else {
        dat <- wqformat::format_results(dat, input$result_format, "masswater")
      }

      val$dat_result <- dat
    }) |>
      bindEvent(input$result_upload)

    # * Sites ----
    
    # dat_sites <- reactive({
    #   req(input$in_sites)
    #   req(input$site_format)
    #   req(val$col_site || input$site_format != "custom")
    #
    #   # Read in file
    #   message("Reading file")
    #   dat <- readxl::read_excel(
    #     input$in_sites,
    #     na = c("NA", "na", ""),
    #     guess_max = Inf
    #   ) |>
    #     dplyr::mutate_if(function(x) !lubridate::is.POSIXct(x), as.character)
    #
    #   in_format <- input$site_format
    #
    #   if (in_format == "masswater") {
    #     return(dat)
    #   } else if (in_format == "custom") {
    #     if (!is.null(val$col_site)) {
    #       dat <- wqformat::rename_col(dat, val$col_site, names(val$col_site))
    #     }
    #
    #     in_format <- "masswater"
    #   }
    #
    #   wqformat::format_sites(dat, in_format, "masswater")
    # }) |>
    #   bindEvent(input$btn_run)

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
