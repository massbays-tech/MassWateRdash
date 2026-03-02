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
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        width = 500,
        # Upload files
        h2("Upload Data"),
        fileInput("resdat", "Upload Results Data (.xlsx)", accept = ".xlsx"),
        fileInput("sitdat", "Upload Site Data (.xlsx)", accept = ".xlsx"),
        fileInput(
          "condat",
          "Upload Conversion Table (.xlsx)",
          accept = ".xlsx"
        ),
        # Convert, download
        h2("Process Data"),
        dropdown(
          ns("in_format"), 
          label = "Input Format", 
          choices = c(
            "MA_BRC", "ME_FOCB", "ME_DEP", "RI_DEM", "RI_WW", "wqdashboard", 
            "WQX", "Custom"
          ), 
          choice_names = c(
            "Blackstone River Coalition", "Friends of Casco Bay", "Maine DEP", 
            "RI DEM", "URI Watershed Watch", "wqdashboard", "WQX", "Custom" 
          ), 
          sorted = FALSE,
          multiple = FALSE
        ),
        actionButton(
          ns("download"),
          "Convert to MassWateR",
          style = "width: fit-content;"
        ),
        downloadButton(
          ns("download"),
          "Download Converted Files",
          style = "width: fit-content;"
        )
      ),
      layout_columns(
        fill = FALSE,
        bslib::value_box(
          title = "Results Data",
          value = "foo"
          # value = htmlOutput(ns("resdat_status"))
        ),
        bslib::value_box(
          title = "Sites Data",
          value = "foo"
          # value = htmlOutput(ns("sitdat_status"))
        ),
        bslib::value_box(
          title = "Conversion Table",
          value = "foo"
          # value = htmlOutput(ns("condat_status"))
        )
      ),
      bslib::card(
        bslib::card_header("Data Validation Messages")#,
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
        resdat = "",
        sitdat = ""
      )
    )
  })
}
