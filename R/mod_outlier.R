#' outlier UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_outlier_ui <- function(id) {
  ns <- NS(id)

  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        title = "Options",
        width = 500,
        selectInput(
          ns("param"),
          "Parameter",
          choices = NULL
        ),
        sliderInput(
          ns("date_range"),
          "Date range",
          min = Sys.Date(),
          max = Sys.Date(),
          value = c(Sys.Date(), Sys.Date()),
          width = '95%'
        ),
        selectInput(
          ns("group"),
          "Group by",
          choices = c("month", "week", "site")
        ),
        selectInput(
          ns("type"),
          "Plot type",
          choices = c("box", "jitterbox", "jitter")
        )
      ),
      bslib::navset_card_underline(
        full_screen = TRUE,
        bslib::nav_panel(
          "Plot",
          plotOutput(ns("outlier_plot"))
        ),
        bslib::nav_panel(
          "Table",
          reactable::reactableOutput(ns("outlier_table"))
        ),
        bslib::nav_panel(
          "Report",
          uiOutput(ns("dwnldoutwrdbutt")),
          uiOutput(ns("dwnldoutzipbutt"))
        )
      )
    )
  )
}

#' outlier Server Functions
#'
#' @noRd
mod_outlier_server <- function(id, fsetls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #Toggle tabset ----
    observe({
      if (!isTruthy(fsetls()$res)) {
        updateTabsetPanel(inputId = "ts_sidebar", selected = "loading")
      } else {
        updateTabsetPanel(inputId = "ts_sidebar", selected = "ready")
      }
    }) |>
      bindEvent(fsetls()$res)

    # reactive UI -----
    observe({
      req(fsetls()$res)

      dat <- fsetls()$res

      tosel <- sort(unique(dat$`Characteristic Name`))

      updateSelectInput(
        session = session,
        inputId = "param",
        choices = tosel
      )
    }) |>
      bindEvent(fsetls()$res)

    observe({
      req(fsetls()$res, input$param)

      param <- input$param

      tosel <- fsetls()$res |>
        dplyr::filter(.data$`Characteristic Name` == param) |>
        dplyr::pull(.data$`Activity Start Date`) |>
        range() |>
        as.Date()

      updateSliderInput(
        session = session,
        inputId = "date_range",
        min = tosel[1],
        max = tosel[2],
        value = tosel
      )
    }) |>
      bindEvent(fsetls()$res, input$param)

    # Plots ----
    output$outlier_plot <- renderPlot({
      # inputs
      param <- input$param
      date_range <- as.character(input$date_range)
      group <- input$group
      type <- input$type

      req(fsetls()$res, fsetls()$acc, param, date_range)

      anlzMWRoutlier(
        res = fsetls()$res,
        param = param,
        acc = fsetls()$acc,
        group = group,
        type = type,
        dtrng = date_range,
        bssize = 18
      ) +
        ggplot2::labs(title = NULL)
    })

    output$outlier_table <- reactable::renderReactable({
      # inputs
      param <- input$param
      date_range <- as.character(input$date_range)
      group <- input$group
      type <- input$type

      req(fsetls()$res, fsetls()$acc, param, date_range)

      tab <- anlzMWRoutlier(
        res = fsetls()$res,
        param = param,
        acc = fsetls()$acc,
        group = group,
        dtrng = date_range,
        outliers = TRUE
      )

      reactable::reactable(
        tab,
        defaultColDef = reactable::colDef(
          footerStyle = list(fontWeight = "bold"),
          resizable = TRUE
        ),
        filterable = TRUE
      )
    })

    # download outlier report word
    output$dwnldoutwrd <- downloadHandler(
      filename = function() {
        "outlierreport.docx"
      },
      content = function(file) {
        # inputs
        date_range <- as.character(input$date_range)
        group <- input$group
        type <- input$type

        anlzMWRoutlierall(
          fset = fsetls(),
          group = group,
          type = type,
          dtrng = date_range,
          format = "word",
          output_dir = dirname(file),
          output_file = basename(file)
        )
      }
    )

    # download outlier report zip
    output$dwnldoutzip <- downloadHandler(
      filename = function() {
        "outlierreport.zip"
      },
      content = function(file) {
        # inputs
        date_range <- as.character(input$date_range)
        group <- input$group
        type <- input$type

        anlzMWRoutlierall(
          fset = fsetls(),
          group = group,
          type = type,
          dtrng = date_range,
          format = "zip",
          output_dir = dirname(file),
          output_file = basename(file)
        )
      }
    )

    output$dwnldoutwrdbutt <- renderUI({
      req(fsetls()$res, fsetls()$acc)

      dl_btn('dwnldoutwrd', 'Download outlier report: Word')
    })
  })
}
