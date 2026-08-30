#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          "Upload Data Files",
          help_btn(ns("show_help"))
        ),
        width = 500,
        div(
          style = "display: flex; align-items: center; gap: 12px;",
          div(
            style = "flex: 0 0 auto;",
            shinyWidgets::materialSwitch(ns("tester"), "Test mode", FALSE)
          ),
          div(
            style = "flex: 1;",
            uiOutput(ns("download_data_btn"))
          )
        ),
        actionButton(
          ns("show_format_modal"),
          "Convert from another format",
          icon = icon("right-left"),
          width = "100%",
          class = "mb-3",
          style = "background-color: #64C147; border-color: #64C147; color: white;"
        ),
        fileInput(
          ns("resdat"),
          "Upload Results Data (.xlsx)",
          accept = ".xlsx"
        ),
        fileInput(
          ns("accdat"),
          "Upload DQO Accuracy Data (.xlsx)",
          accept = ".xlsx"
        ),
        fileInput(
          ns("frecomdat"),
          "Upload DQO Frequency & Completeness Data (.xlsx)",
          accept = ".xlsx"
        ),
        fileInput(
          ns("sitdat"),
          "Upload Site Data (.xlsx)",
          accept = ".xlsx"
        ),
        fileInput(
          ns("wqxdat"),
          "Upload WQX Meta Data (.xlsx)",
          accept = ".xlsx"
        ),
        fileInput(
          ns("censdat"),
          "Upload Censored Data (.xlsx) (optional)",
          accept = ".xlsx"
        )
      ),
      bslib::layout_columns(
        fill = FALSE,
        bslib::value_box(
          title = "Results Data",
          value = htmlOutput(ns("resdat_status"))
        ),
        bslib::value_box(
          title = "Accuracy Data",
          value = htmlOutput(ns("accdat_status"))
        ),
        bslib::value_box(
          title = "Frequency & Completeness Data",
          value = htmlOutput(ns("frecomdat_status"))
        ),
        bslib::value_box(
          title = "Sites Data",
          value = htmlOutput(ns("sitdat_status"))
        ),
        bslib::value_box(
          title = "WQX Data",
          value = htmlOutput(ns("wqxdat_status"))
        ),
        bslib::value_box(
          title = "Censored Data",
          value = htmlOutput(ns("censdat_status"))
        )
      ),
      bslib::card(
        bslib::card_header("Data Validation Messages"),
        uiOutput(ns("validation_messages")),
        mod_upload_repair_ui(ns("resdat_editor"), "resdat"),
        mod_upload_repair_ui(ns("accdat_editor"), "accdat"),
        mod_upload_repair_ui(ns("frecomdat_editor"), "frecomdat"),
        mod_upload_repair_ui(ns("sitdat_editor"), "sitdat"),
        mod_upload_repair_ui(ns("wqxdat_editor"), "wqxdat"),
        mod_upload_repair_ui(ns("censdat_editor"), "censdat")
      )
    )
  )
}

#' upload Server Functions
#'
#' @noRd
mod_upload_server <- function(id, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # R6 classes ----
    val_log <- validationLog$new()
    val_edit <- editVisible$new()
    val_res <- resClass$new()
    val_acc <- accClass$new()
    val_frecom <- frecomClass$new()
    val_sit <- sitClass$new()
    val_wqx <- wqxClass$new()
    val_cens <- censClass$new()

    # Gargoyle watchers ----
    gargoyle::init("update_val")

    # Help modal ----
    mod_tab_help_server(
      "help",
      tab_value = "upload",
      active_tab = active_tab,
      force_show = reactive(input$show_help),
      title = "Upload & Validate",
      body_ui = shiny::includeMarkdown("www/help/upload.md")
    )

    # Modules ----
    wqf <- mod_upload_format_server("reformat")
    mod_upload_repair_server(
      "resdat_editor",
      dat_name = "resdat",
      val_log = val_log,
      val_edit = val_edit,
      val_dat = val_res
    )
    mod_upload_repair_server(
      "accdat_editor",
      dat_name = "accdat",
      val_log = val_log,
      val_edit = val_edit,
      val_dat = val_acc
    )
    mod_upload_repair_server(
      "frecomdat_editor",
      dat_name = "frecomdat",
      val_log = val_log,
      val_edit = val_edit,
      val_dat = val_frecom
    )
    mod_upload_repair_server(
      "sitdat_editor",
      dat_name = "sitdat",
      val_log = val_log,
      val_edit = val_edit,
      val_dat = val_sit
    )
    mod_upload_repair_server(
      "wqxdat_editor",
      dat_name = "wqxdat",
      val_log = val_log,
      val_edit = val_edit,
      val_dat = val_wqx
    )
    mod_upload_repair_server(
      "censdat_editor",
      dat_name = "censdat",
      val_log = val_log,
      val_edit = val_edit,
      val_dat = val_cens
    )

    # Format data ----
    observe({
      req(wqf$dat_results())

      from_format_upload(
        wqf$dat_results(),
        retry_fn = retry_fns$resdat,
        data_name = "resdat",
        val_log = val_log,
        val_edit = val_edit,
        val_dat = val_res
      )
      gargoyle::trigger("update_val")
      showNotification(
        "Results data loaded from format converter",
        type = "message",
        duration = 4
      )
    }) |>
      bindEvent(wqf$dat_results())

    observe({
      req(wqf$dat_sites())

      from_format_upload(
        wqf$dat_sites(),
        retry_fn = retry_fns$sitdat,
        data_name = "sitdat",
        val_log = val_log,
        val_edit = val_edit,
        val_dat = val_sit
      )
      gargoyle::trigger("update_val")
      showNotification(
        "Sites data loaded from format converter",
        type = "message",
        duration = 4
      )
    }) |>
      bindEvent(wqf$dat_sites())

    observe({
      showModal(
        modalDialog(
          title = "Convert from Another Format",
          mod_upload_format_ui(ns("reformat"), in_modal = TRUE),
          size = "xl",
          footer = modalButton("Close"),
          easyClose = TRUE
        )
      )
    }) |>
      bindEvent(input$show_format_modal)

    # Upload & validate -----
    observe({
      fl_upload(
        input$resdat, readMWRresults, "resdat", val_log, val_edit, val_res
      )
      gargoyle::trigger("update_val")
    }) |>
      bindEvent(input$resdat)

    observe({
      fl_upload(
        input$accdat, readMWRacc, "accdat", val_log, val_edit, val_acc
      )
      gargoyle::trigger("update_val")
    }) |>
      bindEvent(input$accdat)

    observe({
      fl_upload(
        input$frecomdat, readMWRfrecom, "frecomdat", val_log, val_edit,
        val_frecom
      )
      gargoyle::trigger("update_val")
    }) |>
      bindEvent(input$frecomdat)

    observe({
      fl_upload(
        input$sitdat, readMWRsites, "sitdat", val_log, val_edit, val_sit
      )
      gargoyle::trigger("update_val")
    }) |>
      bindEvent(input$sitdat)

    observe({
      fl_upload(
        input$wqxdat, readMWRwqx, "wqxdat", val_log, val_edit, val_wqx
      )
      gargoyle::trigger("update_val")
    }) |>
      bindEvent(input$wqxdat)

    observe({
      fl_upload(
        input$censdat, readMWRcens, "censdat", val_log, val_edit, val_cens
      )
      gargoyle::trigger("update_val")
    }) |>
      bindEvent(input$censdat)

    # Validation messages -----
    output$validation_messages <- renderUI({
      gargoyle::watch("update_val")
      format_log(val_log$msg)
    })

    # Data Status ----
    output$resdat_status <- renderUI({
      gargoyle::watch("update_val")
      fl_status(input$tester, input$resdat, val_res$dat)
    })

    output$accdat_status <- renderUI({
      gargoyle::watch("update_val")
      fl_status(input$tester, input$accdat, val_acc$dat)
    })

    output$frecomdat_status <- renderUI({
      gargoyle::watch("update_val")
      fl_status(input$tester, input$frecomdat, val_frecom$dat)
    })

    output$sitdat_status <- renderUI({
      gargoyle::watch("update_val")
      fl_status(input$tester, input$sitdat, val_sit$dat)
    })

    output$wqxdat_status <- renderUI({
      gargoyle::watch("update_val")
      fl_status(input$tester, input$wqxdat, val_wqx$dat)
    })

    output$censdat_status <- renderUI({
      gargoyle::watch("update_val")
      fl_status(input$tester, input$censdat, val_cens$dat)
    })

    # Bundle data ----
    fsetls <- reactive({
      if (input$tester) {
        resdat <- readMWRresults(
          system.file(
            "extdata",
            "ExampleResults.xlsx",
            package = "MassWateR"
          ),
          runchk = FALSE
        )
        accdat <- readMWRacc(
          system.file(
            "extdata",
            "ExampleDQOAccuracy.xlsx",
            package = "MassWateR"
          ),
          runchk = FALSE
        )
        frecomdat <- readMWRfrecom(
          system.file(
            "extdata",
            "ExampleDQOFrequencyCompleteness.xlsx",
            package = "MassWateR"
          ),
          runchk = FALSE
        )
        sitdat <- readMWRsites(
          system.file(
            "extdata",
            "ExampleSites.xlsx",
            package = "MassWateR"
          ),
          runchk = FALSE
        )
        wqxdat <- readMWRwqx(
          system.file(
            "extdata",
            "ExampleWQX.xlsx",
            package = "MassWateR"
          ),
          runchk = FALSE
        )
        censdat <- readMWRcens(
          system.file(
            "extdata",
            "ExampleCensored.xlsx",
            package = "MassWateR"
          ),
          runchk = FALSE
        )
      } else {
        resdat <- val_res$dat
        accdat <- val_acc$dat
        frecomdat <- val_frecom$dat
        sitdat <- val_sit$dat
        wqxdat <- val_wqx$dat
        censdat <- val_cens$dat
      }

      list(
        res = resdat,
        acc = accdat,
        frecom = frecomdat,
        sit = sitdat,
        wqx = wqxdat,
        cens = censdat
      )
    }) |>
      bindEvent(input$tester, gargoyle::watch("update_val"))

    # Download data ----
    output$download_data_btn <- renderUI({
      any_loaded <- isTRUE(input$tester) || !is.null(unlist(fsetls()))
      if (!any_loaded) {
        return(NULL)
      }
      dl_btn(ns("download_data"), "Download data", size = "sm")
    })

    output$download_data <- downloadHandler(
      filename = function() {
        paste0("MassWateR_data_", format(Sys.time(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        fls <- fsetls()
        file_map <- list(
          "results.csv" = fls$res,
          "accuracy.csv" = fls$acc,
          "frequency_completeness.csv" = fls$frecom,
          "sites.csv" = fls$sit,
          "wqx_metadata.csv" = fls$wqx,
          "censored.csv" = fls$cens
        )
        tmp_dir <- tempfile(pattern = "masswater_dl_")
        dir.create(tmp_dir)
        for (nm in names(file_map)) {
          df <- file_map[[nm]]
          if (!is.null(df)) {
            write.csv(df, file.path(tmp_dir, nm), row.names = FALSE)
          }
        }
        old_wd <- setwd(tmp_dir)
        on.exit(setwd(old_wd), add = TRUE)
        utils::zip(file, list.files(tmp_dir))
      }
    )

    # Module output ----
    return(
      reactive({
        fsetls()
      })
    )
  })
}
