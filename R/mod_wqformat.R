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
    # Enable javascript ----
    shinyjs::useShinyjs(),
    # UI ----
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        width = 500,
        # Upload files
        h2("Upload Data"),
        fileInput(
          ns("in_results"),
          "Upload Results Data (.xlsx)",
          accept = ".xlsx"
        ),
        fileInput("in_sites", "Upload Site Data (.xlsx)", accept = ".xlsx"),
        fileInput(
          ns("in_convert"),
          "Upload Conversion Table (.xlsx)",
          accept = ".xlsx"
        ),
        # Convert, download
        h2("Process Data"),
        dropdown(
          ns("result_format"),
          label = "Results Format",
          choices = c(
            "masswater", "MA_BRC", "ME_FOCB", "ME_DEP", "RI_DEM", "RI_WW",
            "wqdashboard", "WQX", "custom"
          ),
          choice_names = c(
            "MassWateR", "Blackstone River Coalition", "Friends of Casco Bay",
            "Maine DEP", "RI DEM", "URI Watershed Watch", "WQdashboard", "WQX",
            "Custom"
          ),
          sorted = FALSE,
          multiple = FALSE
        ),
        dropdown(
          ns("site_format"),
          label = "Site Format",
          choices = c(
            "masswater", "MA_BRC", "ME_FOCB", "RI_WW", "wqdashboard", "WQX",
            "custom"
          ),
          choice_names = c(
            "MassWateR", "Blackstone River Coalition", "Friends of Casco Bay",
            "URI Watershed Watch", "WQdashboard", "WQX", "Custom"
          ),
          sorted = FALSE,
          multiple = FALSE
        ),
        actionButton(
          ns("btn_run"),
          "Convert to MassWateR",
          style = "width: fit-content;"
        ),
        downloadButton(
          ns("btn_download"),
          "Download Converted Files",
          style = "width: fit-content;"
        )
      ),
      bslib::layout_columns(
        fill = FALSE,
        bslib::value_box(
          title = "Results Data",
          value = "foo"
          # value = htmlOutput(ns("status_results"))
        ),
        bslib::value_box(
          title = "Sites Data",
          value = "foo"
          # value = htmlOutput(ns("status_sites"))
        ),
        bslib::value_box(
          title = "Conversion Table",
          value = "foo"
          # value = htmlOutput(ns("status_convert"))
        )
      ),
      bslib::card(
        bslib::card_header("Data Validation Messages") # ,
        # verbatimTextOutput(ns("validation_messages"), placeholder = FALSE)
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
    # Update UI ----
    shinyjs::disable("btn_run")

    observe({
      chk <- input$result_format != "custom" | val$allow_custom
      chk2 <- input$site_format != "custom" | !is.null(val$col_site)

      if (chk & chk2) {
        shinyjs::enable("btn_run")
      } else {
        shinyjs::disable("btn_run")
      }
    })

    # 1 Upload data ----
    val <- reactiveValues(
      allow_custom = FALSE,
      col_site = NULL,
      col_result = NULL,
      var_activity = NULL,
      var_parameter = NULL,
      var_unit = NULL,
      var_qualifier = NULL
    )

    observe({
      req(input$in_convert)
      
      in_dat <- input$in_convert$datapath
      
      print(in_dat)

      val$col_site <- custom_format(in_dat, "Site Columns")
      val$col_result <- custom_format(in_dat, "Result Columns")
      val$var_activity <- custom_format(in_dat, "Activity Types")
      val$var_parameter <- custom_format(in_dat, "Parameters")
      val$var_unit <- custom_format(in_dat, "Units")
      val$var_qualifier <- custom_format(in_dat, "Qualifiers")

      # Check - allow custom result format?
      all_val <- c(
        val$col_result, val$var_activity, val$var_parameter, val$var_unit,
        val$var_qualifier
      )

      if (is.null(all_val)) {
        val$allow_custom <- FALSE
      } else {
        val$allow_custom <- TRUE
      }
    }) |>
      bindEvent(input$in_convert)

    # 2 Format data -----
    dat_results <- reactive({
      req(input$in_results)
      req(input$result_format)
      req(val$allow_custom || input$result_format != "custom")

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

        return(dat)
      } else if (input$result_format == "custom") {
        dat <- convert_results(
          dat,
          col_names = val$col_result,
          var_activity = val$var_activity,
          var_param = val$var_parameter,
          var_unit = val$var_unit,
          var_qualifier = val$var_qualifier
        )

        return(dat)
      }

      wqformat::format_results(dat, input$result_format, "masswater")
    }) |>
      bindEvent(input$btn_run)

    dat_sites <- reactive({
      req(input$in_sites)
      req(input$site_format)
      req(val$col_site || input$site_format != "custom")

      # Read in file
      message("Reading file")
      dat <- readxl::read_excel(
        input$in_sites,
        na = c("NA", "na", ""),
        guess_max = Inf
      ) |>
        dplyr::mutate_if(function(x) !lubridate::is.POSIXct(x), as.character)

      in_format <- input$site_format

      if (in_format == "masswater") {
        return(dat)
      } else if (in_format == "custom") {
        if (!is.null(val$col_site)) {
          dat <- wqformat::rename_col(dat, val$col_site, names(val$col_site))
        }

        in_format <- "masswater"
      }

      wqformat::format_sites(dat, in_format, "masswater")
    }) |>
      bindEvent(input$btn_run)

    # 3 Download ----


    # # upload & validate -----
    # validation_log <<- reactiveVal("")
    # data_states <<- reactiveValues(
    #   resdat = NULL,
    #   sitdat = NULL,
    #   condat = NULL
    # )
    #
    # # Observers for each data upload
    # observeEvent(input$resdat, {
    #   fl_upload(input$resdat, readMWRresults, "resdat")
    # })
    #
    # observeEvent(input$accdat, {
    #   fl_upload(input$accdat, readMWRacc, "accdat")
    # })
    #
    # observeEvent(input$frecomdat, {
    #   fl_upload(input$frecomdat, readMWRfrecom, "frecomdat")
    # })
    #
    # observeEvent(input$sitdat, {
    #   fl_upload(input$sitdat, readMWRsites, "sitdat")
    # })
    #
    # observeEvent(input$wqxdat, {
    #   fl_upload(input$wqxdat, readMWRwqx, "wqxdat")
    # })
    #
    # # Status outputs
    # output$resdat_status <- renderUI({
    #   fl_status(input$tester, input$resdat, data_states$resdat)
    # })
    #
    # output$accdat_status <- renderUI({
    #   fl_status(input$tester, input$accdat, data_states$accdat)
    # })
    #
    # output$frecomdat_status <- renderUI({
    #   fl_status(input$tester, input$frecomdat, data_states$frecomdat)
    # })
    #
    # output$sitdat_status <- renderUI({
    #   fl_status(input$tester, input$sitdat, data_states$sitdat)
    # })
    #
    # output$wqxdat_status <- renderUI({
    #   fl_status(input$tester, input$wqxdat, data_states$wqxdat)
    # })
    #
    # # Output validation messages
    # output$validation_messages <- renderText({
    #   validation_log()
    # })
    #
    # # data inputs
    # fsetls <- reactive({
    #   resdat <- data_states$resdat
    #   sitdat <- data_states$sitdat
    #   condat <- data_states$condat
    #
    #   list(
    #     res = resdat,
    #     sit = sitdat,
    #     con = condat
    #   )
    # })

    # Return data ----
    return(
      list(
        dat_results = reactive({
          dat_results()
        }),
        dat_sites = ""
      )
    )
  })
}
