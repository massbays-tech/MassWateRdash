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
      # Sidebar ----
      sidebar = bslib::sidebar(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          "Plot options",
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
        dropdown(ns("sites"), "Select sites", choices = NULL),
        conditionalPanel(
          condition = paste0('output["', ns("tab_name"), '"] != "Map"'),
          selectInput(ns("thresh"), "Threshold type", choices = "none")
        ),
        conditionalPanel(
          condition = paste0(
            '["Season", "Site"].includes(output["',
            ns("tab_name"),
            '"])'
          ),
          selectInput(
            ns("type"),
            "Plot type",
            choices = c("box", "jitterbox", "bar", "jitterbar", "jitter")
          )
        ),
        conditionalPanel(
          condition = paste0('output["', ns("tab_name"), '"] == "Date"'),
          selectInput(
            ns("group"),
            "Plot grouping",
            choices = c("site", "locgroup", "all")
          )
        ),
        conditionalPanel(
          condition = paste0('output["', ns("show_conf"), '"] == "TRUE"'),
          selectInput(ns("confint"), "Show confidence", choices = c(F, T))
        ),
        actionButton(
          ns("open_plot_download"),
          "Download plot",
          icon = icon("download"),
          width = "100%",
          style = "background-color: #64C147; border-color: #64C147; color: white;"
        )
      ),
      # Navset card ----
      bslib::navset_card_underline(
        full_screen = T,
        id = ns("viz_selected"),
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
              "none" = "NULL", "OpenStreetMap", "OpenStreetMap.DE",
              "OpenStreetMap.France", "OpenStreetMap.HOT", "OpenTopoMap",
              "Esri.WorldStreetMap", "Esri.DeLorme", "Esri.WorldTopoMap",
              "Esri.WorldImagery", "Esri.WorldTerrain",
              "Esri.WorldShadedRelief", "Esri.OceanBasemap",
              "Esri.NatGeoWorldMap", "Esri.WorldGrayCanvas", "CartoDB.Positron",
              "CartoDB.PositronNoLabels", "CartoDB.PositronOnlyLabels",
              "CartoDB.DarkMatter", "CartoDB.DarkMatterNoLabels",
              "CartoDB.DarkMatterOnlyLabels", "CartoDB.Voyager",
              "CartoDB.VoyagerNoLabels", "CartoDB.VoyagerOnlyLabels"
            )
          ),
          plotOutput(ns("map_plot"))
        )
      )
    )
  )
}

#' visualize Server Functions
#'
#' @noRd
mod_visualize_server <- function(id, fsetls, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Help modal ----
    mod_tab_help_server(
      "help",
      tab_value = "visualize",
      active_tab = active_tab,
      force_show = reactive(input$show_help),
      title = "Visualize",
      body_ui = shiny::includeMarkdown(app_sys("app/www/help/visualize.md"))
    )

    # Update sidebar UI-----
    observe({
      req(fsetls()$res)

      tosel <- sort(unique(fsetls()$res$`Characteristic Name`))

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

    observe({
      param <- input$param
      date_range <- input$date_range
      req(fsetls()$res, param, date_range)

      valid_sites <- fsetls()$res |>
        dplyr::filter(
          .data$`Characteristic Name` == param,
          .data$`Activity Start Date` >= date_range[1],
          .data$`Activity Start Date` <= date_range[2]
        ) |>
        dplyr::pull(.data$`Monitoring Location ID`) |>
        unique() |>
        sort()

      shinyWidgets::updatePickerInput(
        inputId = "sites",
        choices = valid_sites,
        selected = valid_sites
      )
    }) |>
      bindEvent(fsetls()$res, input$param, input$date_range)

    output$tab_name <- renderText({
      input$viz_selected
    })
    outputOptions(output, "tab_name", suspendWhenHidden = FALSE)

    observe({
      param <- input$param
      req(param)

      # Characteristic Name in the results file == Simple Parameter in thresholdMWR,
      # so filter directly without going through paramsMWR
      thresh_rows <- MassWateR::thresholdMWR |>
        dplyr::filter(.data$`Simple Parameter` == param)

      has_fresh <- nrow(thresh_rows) > 0 &&
        any(!is.na(thresh_rows$Fresh_1) | !is.na(thresh_rows$Fresh_2))
      has_marine <- nrow(thresh_rows) > 0 &&
        any(!is.na(thresh_rows$Marine_1) | !is.na(thresh_rows$Marine_2))

      choices <- c(
        if (has_fresh) "fresh",
        if (has_marine) "marine",
        "none"
      )

      updateSelectInput(
        session = session,
        inputId = "thresh",
        choices = choices
      )
    }) |>
      bindEvent(input$param)

    output$show_conf <- renderText({
      viz <- input$viz_selected

      show <- if (viz %in% c("Season", "Site")) {
        isTRUE(input$type %in% c("bar", "jitterbar"))
      } else if (viz == "Date") {
        isTRUE(input$group %in% c("locgroup", "all"))
      } else {
        FALSE
      }

      paste(show)
    })
    outputOptions(output, "show_conf", suspendWhenHidden = FALSE)

    # Download ----
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
          input$param,
          "_",
          tolower(input$viz_selected),
          ".",
          input$plot_format
        )
      },
      content = function(file) {
        fset <- fsetls()
        viz <- input$viz_selected
        param <- input$param
        date_range <- as.character(input$date_range)
        sites <- input$sites
        thresh <- if (is.null(input$thresh)) "none" else input$thresh
        confint <- isTRUE(as.logical(input$confint))

        p <- if (viz == "Season") {
          anlzMWRseason(
            res = fset$res,
            param = param,
            acc = fset$acc,
            sit = fset$sit,
            thresh = thresh,
            type = input$type,
            dtrng = date_range,
            site = sites,
            confint = confint,
            bssize = 18
          ) +
            ggplot2::labs(title = NULL)
        } else if (viz == "Date") {
          anlzMWRdate(
            res = fset$res,
            param = param,
            acc = fset$acc,
            sit = fset$sit,
            thresh = thresh,
            group = input$group,
            dtrng = date_range,
            site = sites,
            confint = confint,
            bssize = 18
          ) +
            ggplot2::labs(title = NULL)
        } else if (viz == "Site") {
          anlzMWRsite(
            res = fset$res,
            param = param,
            acc = fset$acc,
            sit = fset$sit,
            thresh = thresh,
            type = input$type,
            dtrng = date_range,
            site = sites,
            confint = confint,
            bssize = 18
          ) +
            ggplot2::labs(title = NULL)
        } else {
          watsel <- if (isTRUE(input$watsel == "NULL")) NULL else input$watsel
          mapsel <- if (isTRUE(input$mapsel == "NULL")) NULL else input$mapsel
          anlzMWRmap(
            res = fset$res,
            param = param,
            acc = fset$acc,
            sit = fset$sit,
            dtrng = date_range,
            site = sites,
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

    # Plots ----
    output$season_plot <- renderPlot({
      # inputs
      thresh <- if (is.null(input$thresh)) "none" else input$thresh
      param <- input$param
      date_range <- as.character(input$date_range)
      sites <- input$sites
      type <- input$type
      confint <- isTRUE(as.logical(input$confint))

      req(fsetls()$res, fsetls()$acc, param, date_range, sites)

      anlzMWRseason(
        res = fsetls()$res,
        param = param,
        acc = fsetls()$acc,
        sit = fsetls()$sit,
        thresh = thresh,
        type = type,
        dtrng = date_range,
        site = sites,
        confint = confint,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = NULL)
    })

    output$date_plot <- renderPlot({
      # inputs
      thresh <- if (is.null(input$thresh)) "none" else input$thresh
      param <- input$param
      date_range <- as.character(input$date_range)
      sites <- input$sites
      group <- input$group
      confint <- isTRUE(as.logical(input$confint))

      req(fsetls()$res, fsetls()$acc, param, date_range, sites)

      anlzMWRdate(
        res = fsetls()$res,
        param = param,
        acc = fsetls()$acc,
        sit = fsetls()$sit,
        thresh = thresh,
        group = group,
        dtrng = date_range,
        site = sites,
        confint = confint,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = NULL)
    })

    output$site_plot <- renderPlot({
      # inputs
      thresh <- if (is.null(input$thresh)) "none" else input$thresh
      param <- input$param
      date_range <- as.character(input$date_range)
      sites <- input$sites
      type <- input$type
      confint <- isTRUE(as.logical(input$confint))

      req(fsetls()$res, fsetls()$acc, param, date_range, sites)

      anlzMWRsite(
        res = fsetls()$res,
        param = param,
        acc = fsetls()$acc,
        sit = fsetls()$sit,
        thresh = thresh,
        type = type,
        dtrng = date_range,
        site = sites,
        confint = confint,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = NULL)
    })

    output$map_plot <- renderPlot({
      # inputs
      param <- input$param
      date_range <- as.character(input$date_range)
      sites <- input$sites
      watsel <- input$watsel
      mapsel <- input$mapsel

      req(fsetls()$res, fsetls()$acc, fsetls()$sit, param, date_range, sites)

      if (watsel == "NULL") {
        watsel <- NULL
      }
      if (mapsel == "NULL") {
        mapsel <- NULL
      }

      anlzMWRmap(
        res = fsetls()$res,
        param = param,
        acc = fsetls()$acc,
        sit = fsetls()$sit,
        dtrng = date_range,
        site = sites,
        addwater = watsel,
        maptype = mapsel,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = NULL)
    })
  })
}
