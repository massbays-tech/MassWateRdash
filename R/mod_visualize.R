#' visualize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visualize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        title = "Plot options",
        width = 500,
        uiOutput(ns("prm2")),
        uiOutput(ns("dtrng2")),
        uiOutput(ns("sites2")),
        uiOutput(ns("notmap")),
        uiOutput(ns("vizui")),
        uiOutput(ns("confint_ui")),
        uiOutput(ns("download_plot_btn"))
      ),
      bslib::navset_card_underline(
        full_screen = T,
        id = "viz_selected",
        bslib::nav_panel(
          "Season",
          plotOutput(ns("season_plot"))
        ),
        bslib::nav_panel(
          "Date",
          plotOutput(ns("date_plot"))
        ),
        bslib::nav_panel(
          "Site",
          plotOutput(ns("site_plot"))
        ),
        bslib::nav_panel(
          "Map",
          selectInput(
            ns("watsel"),
            "Water feature detail",
            choices = c("low", "medium", "high", "none" = "NULL")
          ),
          selectInput(
            ns("mapsel"),
            "Basemap selection",
            choices = c(
              "none" = "NULL",
              "OpenStreetMap",
              "OpenStreetMap.DE",
              "OpenStreetMap.France",
              "OpenStreetMap.HOT",
              "OpenTopoMap",
              "Esri.WorldStreetMap",
              "Esri.DeLorme",
              "Esri.WorldTopoMap",
              "Esri.WorldImagery",
              "Esri.WorldTerrain",
              "Esri.WorldShadedRelief",
              "Esri.OceanBasemap",
              "Esri.NatGeoWorldMap",
              "Esri.WorldGrayCanvas",
              "CartoDB.Positron",
              "CartoDB.PositronNoLabels",
              "CartoDB.PositronOnlyLabels",
              "CartoDB.DarkMatter",
              "CartoDB.DarkMatterNoLabels",
              "CartoDB.DarkMatterOnlyLabels",
              "CartoDB.Voyager",
              "CartoDB.VoyagerNoLabels",
              "CartoDB.VoyagerOnlyLabels"
            )
          ),
          plotOutput("map_plot")
        )
      )
    )
  )
}

#' visualize Server Functions
#'
#' @noRd
mod_visualize_server <- function(id, fsetls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # reactive UI -----

    output$prm2 <- renderUI({
      # inputs
      fset <- fsetls()

      validate(
        need(!is.null(fset$res), 'Waiting for input data...')
      )

      tosel <- sort(unique(fset$res$`Characteristic Name`))

      selectInput(ns("param2"), "Parameter", choices = tosel)
    })

    output$dtrng2 <- renderUI({
      # inputs
      param2 <- input$param2

      req(fsetls$res(), param2)

      tosel <- fsetls$res() |>
        dplyr::filter(.data$`Characteristic Name` == param2) |>
        dplyr::pull(.data$`Activity Start Date`) |>
        range() |>
        as.Date()

      sliderInput(
        ns("dtrng2"),
        "Date range",
        min = tosel[1],
        max = tosel[2],
        value = tosel,
        width = '95%'
      )
    })

    # Reactive: valid sites for the current param2 + date range.
    # Shared by output$sites2 (to populate choices) and the plot guards
    # (to block rendering when input$sites2 is still stale after a param change).
    valid_sites2 <- reactive({
      param2 <- input$param2
      dtrng2 <- input$dtrng2
      req(fsetls$res(), param2, dtrng2)
      fsetls$res() |>
        dplyr::filter(
          .data$`Characteristic Name` == param2,
          .data$`Activity Start Date` >= dtrng2[1],
          .data$`Activity Start Date` <= dtrng2[2]
        ) |>
        dplyr::pull(.data$`Monitoring Location ID`) |>
        unique()
    })

    output$sites2 <- renderUI({
      dropdown("sites2", "Select sites", choices = valid_sites2())
    })

    output$notmap <- renderUI({
      if (input$viz_selected == 'Map') {
        return(NULL)
      }

      param2 <- input$param2
      req(param2)

      # Characteristic Name in the results file == Simple Parameter in thresholdMWR,
      # so filter directly without going through paramsMWR
      thresh_rows <- MassWateR::thresholdMWR |>
        dplyr::filter(.data$`Simple Parameter` == param2)

      has_fresh <- nrow(thresh_rows) > 0 &&
        any(!is.na(thresh_rows$Fresh_1) | !is.na(thresh_rows$Fresh_2))
      has_marine <- nrow(thresh_rows) > 0 &&
        any(!is.na(thresh_rows$Marine_1) | !is.na(thresh_rows$Marine_2))

      # Hide entirely when no thresholds exist for this parameter
      if (!has_fresh && !has_marine) {
        return(NULL)
      }

      choices <- c(
        if (has_fresh) 'fresh',
        if (has_marine) 'marine',
        'none'
      )

      selectInput("thresh", "Threshold type", choices = choices)
    })

    output$vizui <- renderUI({
      out <- NULL

      if (input$viz_selected %in% c('Season', 'Site')) {
        out <- selectInput(
          ns("type2"),
          "Plot type",
          choices = c("box", "jitterbox", "bar", "jitterbar", "jitter")
        )
      }

      if (input$viz_selected == 'Date') {
        out <- selectInput(
          ns("group2"),
          "Plot grouping",
          choices = c("site", "locgroup", "all")
        )
      }

      return(out)
    })

    output$confint_ui <- renderUI({
      viz <- input$viz_selected

      show <- if (viz %in% c('Season', 'Site')) {
        isTRUE(input$type2 %in% c('bar', 'jitterbar'))
      } else if (viz == 'Date') {
        isTRUE(input$group2 %in% c('locgroup', 'all'))
      } else {
        FALSE
      }

      if (show) {
        selectInput(ns("confint2"), "Show confidence", choices = c(F, T))
      }
    })

    output$dwnldoutwrdbutt <- renderUI({
      req(fsetls$res(), fsetls$acc())

      dl_btn(ns('dwnldoutwrd'), 'Download outlier report: Word')
    })

    output$dwnldoutzipbutt <- renderUI({
      req(fsetls$res(), fsetls$acc())

      dl_btn(ns('dwnldoutzip'), 'Download outlier report: Zipped images')
    })

    output$dwnldqcbutt <- renderUI({
      req(fsetls$res(), fsetls$acc(), fsetls$frecom())

      dl_btn(ns('dwnldqc'), 'Download quality control report')
    })

    output$dwnldwqxbutt <- renderUI({
      req(req(fsetls$res(), fsetls$acc(), fsetls$sit(), fsetls$wqx()))

      dl_btn(ns('dwnldwqx'), 'Download WQX workbook')
    })

    output$download_plot_btn <- renderUI({
      req(fsetls$res(), input$param2)
      actionButton(
        ns("open_plot_download"),
        "Download plot",
        icon = icon("download"),
        width = "100%",
        style = "background-color: #64C147; border-color: #64C147; color: white;"
      )
    })

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
        selectInput(
          ns("plot_format"),
          "Format",
          choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg")
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
        paste0(
          input$param2,
          "_",
          tolower(input$viz_selected),
          ".",
          input$plot_format
        )
      },
      content = function(file) {
        fset <- fsetls()
        viz <- input$viz_selected
        param2 <- input$param2
        dtrng2 <- as.character(input$dtrng2)
        sites2 <- input$sites2
        thresh <- if (is.null(input$thresh)) "none" else input$thresh
        confint2 <- isTRUE(as.logical(input$confint2))

        p <- if (viz == "Season") {
          anlzMWRseason(
            res = fset$res,
            param = param2,
            acc = fset$acc,
            sit = fset$sit,
            thresh = thresh,
            type = input$type2,
            dtrng = dtrng2,
            site = sites2,
            confint = confint2,
            bssize = 18
          ) +
            ggplot2::labs(title = NULL)
        } else if (viz == "Date") {
          anlzMWRdate(
            res = fset$res,
            param = param2,
            acc = fset$acc,
            sit = fset$sit,
            thresh = thresh,
            group = input$group2,
            dtrng = dtrng2,
            site = sites2,
            confint = confint2,
            bssize = 18
          ) +
            ggplot2::labs(title = NULL)
        } else if (viz == "Site") {
          anlzMWRsite(
            res = fset$res,
            param = param2,
            acc = fset$acc,
            sit = fset$sit,
            thresh = thresh,
            type = input$type2,
            dtrng = dtrng2,
            site = sites2,
            confint = confint2,
            bssize = 18
          ) +
            ggplot2::labs(title = NULL)
        } else {
          watsel <- if (isTRUE(input$watsel == "NULL")) NULL else input$watsel
          mapsel <- if (isTRUE(input$mapsel == "NULL")) NULL else input$mapsel
          anlzMWRmap(
            res = fset$res,
            param = param2,
            acc = fset$acc,
            sit = fset$sit,
            dtrng = dtrng2,
            site = sites2,
            addwater = watsel,
            maptype = mapsel,
            bssize = 18
          ) +
            ggplot2::labs(title = NULL)
        }

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

    output$season_plot <- renderPlot({
      # inputs
      thresh <- if (is.null(input$thresh)) "none" else input$thresh
      param2 <- input$param2
      dtrng2 <- as.character(input$dtrng2)
      sites2 <- input$sites2
      type2 <- input$type2
      confint2 <- isTRUE(as.logical(input$confint2))

      req(fsetls$res(), fsetls$acc(), param2, dtrng2, sites2)
      req(all(sites2 %in% valid_sites2()))

      anlzMWRseason(
        res = fsetls$res(),
        param = param2,
        acc = fsetls$acc(),
        sit = fsetls$sit(),
        thresh = thresh,
        type = type2,
        dtrng = dtrng2,
        site = sites2,
        confint = confint2,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = NULL)
    })

    output$date_plot <- renderPlot({
      # inputs
      thresh <- if (is.null(input$thresh)) "none" else input$thresh
      param2 <- input$param2
      dtrng2 <- as.character(input$dtrng2)
      sites2 <- input$sites2
      group2 <- input$group2
      confint2 <- isTRUE(as.logical(input$confint2))

      req(fsetls$res(), fsetls$acc(), param2, dtrng2, sites2)
      req(all(sites2 %in% valid_sites2()))

      anlzMWRdate(
        res = fsetls$res(),
        param = param2,
        acc = fsetls$acc(),
        sit = fsetls$sit(),
        thresh = thresh,
        group = group2,
        dtrng = dtrng2,
        site = sites2,
        confint = confint2,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = NULL)
    })

    output$site_plot <- renderPlot({
      # inputs
      thresh <- if (is.null(input$thresh)) "none" else input$thresh
      param2 <- input$param2
      dtrng2 <- as.character(input$dtrng2)
      sites2 <- input$sites2
      type2 <- input$type2
      confint2 <- isTRUE(as.logical(input$confint2))

      req(fsetls$res(), fsetls$acc(), param2, dtrng2, sites2)
      req(all(sites2 %in% valid_sites2()))

      anlzMWRsite(
        res = fsetls$res(),
        param = param2,
        acc = fsetls$acc(),
        sit = fsetls$sit(),
        thresh = thresh,
        type = type2,
        dtrng = dtrng2,
        site = sites2,
        confint = confint2,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = NULL)
    })

    output$map_plot <- renderPlot({
      # inputs
      param2 <- input$param2
      dtrng2 <- as.character(input$dtrng2)
      sites2 <- input$sites2
      watsel <- input$watsel
      mapsel <- input$mapsel

      req(fsetls$res(), fsetls$acc(), fsetls$sit(), param2, dtrng2, sites2)
      req(all(sites2 %in% valid_sites2()))

      if (watsel == "NULL") {
        watsel <- NULL
      }
      if (mapsel == "NULL") {
        mapsel <- NULL
      }

      anlzMWRmap(
        res = fsetls$res(),
        param = param2,
        acc = fsetls$acc(),
        sit = fsetls$sit(),
        dtrng = dtrng2,
        site = sites2,
        addwater = watsel,
        maptype = mapsel,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = NULL)
    })
  })
}
