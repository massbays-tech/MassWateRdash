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
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          "Options",
          help_btn(ns("show_help"))
        ),
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
          width = "95%"
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
        ),
        actionButton(
          ns("open_plot_download"),
          "Download plot",
          icon = icon("download"),
          width = "100%",
          style = "background-color: #64C147; border-color: #64C147; color: white;"
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
          dl_btn(ns("dl_word"), "Download outlier report: Word"),
          dl_btn(ns("dl_zip"), "Download outlier report: Zipped images")
        )
      )
    )
  )
}

#' outlier Server Functions
#'
#' @noRd
mod_outlier_server <- function(id, fsetls, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Help modal ----
    mod_tab_help_server(
      "help",
      tab_value = "outlier",
      active_tab = active_tab,
      force_show = reactive(input$show_help),
      title = "Outlier Assessment",
      body_ui = shiny::includeMarkdown(app_sys("app/www/help/outlier.md"))
    )

    # Update UI -----
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

    # Plot, table ----
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
        pagination = FALSE
      )
    })

    # Download plot ----
    observe({
      showModal(modalDialog(
        title = "Download plot",
        size = "s",
        numericInput(
          ns("plot_width"),
          "Width (inches)",
          value = 10,
          min = 1,
          max = 30
        ),
        numericInput(
          ns("plot_height"),
          "Height (inches)",
          value = 6,
          min = 1,
          max = 30
        ),
        numericInput(
          ns("plot_dpi"),
          "Resolution (DPI)",
          value = 150,
          min = 72,
          max = 600
        ),
        numericInput(
          ns("plot_fontsize"),
          "Font size",
          value = 18,
          min = 6,
          max = 36
        ),
        selectInput(
          ns("plot_format"),
          "Format",
          choices = c("PNG" = "png", "JPG" = "jpg", "PDF" = "pdf", "SVG" = "svg")
        ),
        footer = tagList(
          dl_btn(ns("download_plot"), "Download"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    }) |>
      bindEvent(input$open_plot_download)

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0(input$param, "_outlier.", input$plot_format)
      },
      content = function(file) {
        param <- input$param
        date_range <- as.character(input$date_range)
        group <- input$group
        type <- input$type

        p <- anlzMWRoutlier(
          res = fsetls()$res,
          param = param,
          acc = fsetls()$acc,
          group = group,
          type = type,
          dtrng = date_range,
          bssize = input$plot_fontsize
        ) +
          ggplot2::labs(title = NULL)

        ggplot2::ggsave(
          filename = file,
          plot = p,
          width = input$plot_width,
          height = input$plot_height,
          dpi = input$plot_dpi,
          device = input$plot_format
        )
      }
    )

    # Download ----
    output$dl_word <- downloadHandler(
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

    output$dl_zip <- downloadHandler(
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
  })
}
