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
          condition = paste0(
            'output["', ns("has_locgroup"), '"] == "TRUE"'
          ),
          dropdown(ns("locgroup"), "Location group", choices = NULL)
        ),
        conditionalPanel(
          condition = paste0(
            'output["', ns("has_resultatt"), '"] == "TRUE"'
          ),
          dropdown(ns("resultatt"), "Result attribute", choices = NULL)
        ),
        conditionalPanel(
          condition = paste0(
            'output["', ns("has_resultatt"), '"] == "TRUE" && output["',
            ns("tab_name"), '"] == "Site"'
          ),
          checkboxInput(
            ns("byresultatt"), "Group sites by result attribute"
          )
        ),
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
            choices = c("site", "Location group" = "locgroup", "all")
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
              "Esri.WorldStreetMap", "Esri.WorldTopoMap",
              "Esri.WorldImagery", "Esri.WorldTerrain",
              "Esri.WorldShadedRelief", "Esri.OceanBasemap",
              "Esri.NatGeoWorldMap", "Esri.WorldGrayCanvas"
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

      freezeReactiveValue(input, "param")
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

      freezeReactiveValue(input, "date_range")
      updateSliderInput(
        session = session,
        inputId = "date_range",
        min = tosel[1],
        max = tosel[2],
        value = tosel
      )
    }) |>
      bindEvent(fsetls()$res, input$param)

    # only the rows the MassWateR plot functions use, i.e. surface field
    # samples, so QC rows don't add sites or result attributes that can never
    # appear in a plot
    plot_res <- reactive({
      req(fsetls()$res)

      fsetls()$res |>
        MassWateR::utilMWRfiltersurface() |>
        dplyr::filter(
          .data$`Activity Type` %in% c("Field Msr/Obs", "Sample-Routine")
        )
    })

    # rows for the current parameter and date range, NULL until those exist
    param_rows <- reactive({
      param <- input$param
      date_range <- input$date_range

      if (is.null(fsetls()$res) || is.null(param) || is.null(date_range)) {
        return(NULL)
      }

      plot_res() |>
        dplyr::filter(
          .data$`Characteristic Name` == param,
          .data$`Activity Start Date` >= date_range[1],
          .data$`Activity Start Date` <= date_range[2]
        )
    })

    # same rows narrowed to the selected sites
    site_rows <- reactive({
      rows <- param_rows()
      if (is.null(rows)) {
        return(NULL)
      }

      rows |>
        dplyr::filter(.data$`Monitoring Location ID` %in% input$sites)
    })

    valid_sites_calc <- reactive({
      req(param_rows())

      param_rows() |>
        dplyr::pull(.data$`Monitoring Location ID`) |>
        unique() |>
        sort()
    })

    observe({
      valid_sites <- valid_sites_calc()

      freezeReactiveValue(input, "sites")
      shinyWidgets::updatePickerInput(
        inputId = "sites",
        choices = valid_sites,
        selected = valid_sites
      )
    }) |>
      bindEvent(valid_sites_calc())

    # Location group ----
    # Sites and location groups are linked both ways. A group is selected when
    # any of its sites is selected, and adding or removing a group adds or
    # removes its sites. Sites with no location group, or missing from the
    # site file, fall under "None", so every site belongs to one choice.
    none_lab <- "None"

    is_blank <- function(x) {
      is.na(x) | trimws(x) == ""
    }

    site_groups <- reactive({
      sit <- fsetls()$sit
      rows <- param_rows()

      if (is.null(sit) || !"Location Group" %in% names(sit) || is.null(rows)) {
        return(NULL)
      }

      sitgrp <- sit |>
        dplyr::filter(!is_blank(.data$`Location Group`)) |>
        dplyr::distinct(.data$`Monitoring Location ID`, .data$`Location Group`)

      dplyr::tibble(
        `Monitoring Location ID` = unique(rows$`Monitoring Location ID`)
      ) |>
        dplyr::left_join(sitgrp, by = "Monitoring Location ID") |>
        dplyr::mutate(
          `Location Group` = ifelse(
            is.na(.data$`Location Group`), none_lab, .data$`Location Group`
          )
        )
    })

    # real groups sorted with "None" last, empty when there are no real groups
    # so the dropdown stays hidden
    locgroup_vals <- reactive({
      sg <- site_groups()
      if (is.null(sg)) {
        return(character(0))
      }

      grps <- unique(sg$`Location Group`)
      real <- sort(setdiff(grps, none_lab))
      if (length(real) == 0) {
        return(character(0))
      }

      c(real, if (none_lab %in% grps) none_lab)
    })

    groups_of <- function(sites) {
      sg <- site_groups()
      intersect(
        locgroup_vals(),
        sg$`Location Group`[sg$`Monitoring Location ID` %in% sites]
      )
    }

    sites_in <- function(groups) {
      sg <- site_groups()
      sg$`Monitoring Location ID`[sg$`Location Group` %in% groups]
    }

    output$has_locgroup <- renderText({
      paste(length(locgroup_vals()) > 0)
    })
    outputOptions(output, "has_locgroup", suspendWhenHidden = FALSE)

    # Each update pushed to one dropdown comes back from the browser as an
    # input change. With quick clicks that echo can arrive after the user has
    # already made another change, and treating it as a user edit would push
    # stale values back to the other dropdown. So pushed values are
    # remembered and their echoes ignored. Entries expire in case the browser
    # never echoes, which happens when the pushed value matches what it
    # already has. The inputs aren't frozen since that drops clicks made
    # before the echo arrives.
    pushed_sites <- reactiveVal(list())
    pushed_locgroup <- reactiveVal(list())

    live_pushes <- function(rv) {
      Filter(
        function(p) difftime(Sys.time(), p$time, units = "secs") < 5,
        rv()
      )
    }

    track_push <- function(rv, val) {
      rv(c(live_pushes(rv), list(list(val = val, time = Sys.time()))))
    }

    # drops the matching push and any older ones, the browser applies
    # updates in order
    is_echo <- function(rv, val) {
      pushes <- live_pushes(rv)
      hit <- which(vapply(pushes, function(p) setequal(p$val, val), logical(1)))
      if (length(hit) == 0) {
        rv(pushes)
        return(FALSE)
      }

      rv(pushes[-seq_len(hit[1])])
      TRUE
    }

    # sites -> groups, also refreshes the choices
    locgroup_choices <- reactiveVal(NULL)
    observe({
      vals <- locgroup_vals()
      sel <- if (is_echo(pushed_sites, input$sites)) {
        intersect(input$locgroup, vals)
      } else {
        groups_of(input$sites)
      }

      if (
        identical(vals, locgroup_choices()) && setequal(sel, input$locgroup)
      ) {
        return()
      }

      locgroup_choices(vals)
      track_push(pushed_locgroup, sel)
      shinyWidgets::updatePickerInput(
        inputId = "locgroup",
        choices = vals,
        selected = sel
      )
    }) |>
      bindEvent(locgroup_vals(), input$sites, ignoreNULL = FALSE)

    # groups -> sites, only acts on user edits to the group dropdown
    observe({
      sel <- input$locgroup
      if (is_echo(pushed_locgroup, sel)) {
        return()
      }

      # start from the latest pushed sites if their echo hasn't arrived yet
      pending <- live_pushes(pushed_sites)
      base <- if (length(pending) > 0) {
        pending[[length(pending)]]$val
      } else {
        input$sites
      }
      cur <- groups_of(base)

      if (setequal(sel, cur)) {
        return()
      }

      sites <- setdiff(base, sites_in(setdiff(cur, sel)))
      sites <- union(sites, sites_in(setdiff(sel, cur)))
      valid_sites <- valid_sites_calc()
      sites <- valid_sites[valid_sites %in% sites]

      track_push(pushed_sites, sites)
      shinyWidgets::updatePickerInput(
        inputId = "sites",
        selected = sites
      )
    }) |>
      bindEvent(input$locgroup, ignoreNULL = FALSE, ignoreInit = TRUE)

    observe({
      choices <- if (length(locgroup_vals()) > 0) {
        c("site", "location group" = "locgroup", "all")
      } else {
        c("site", "all")
      }

      selected <- if (isTRUE(input$group %in% choices)) input$group else "site"

      freezeReactiveValue(input, "group")
      updateSelectInput(
        session = session,
        inputId = "group",
        choices = choices,
        selected = selected
      )
    }) |>
      bindEvent(locgroup_vals())

    # Result attribute ----
    # The dropdown shows when the parameter and date range have any result
    # attributes, with "None" for rows that have no entry. The choices narrow
    # to the selected sites, one way only since an attribute applies to rows
    # and doesn't map onto whole sites.
    att_label <- function(x) {
      ifelse(is_blank(x), none_lab, trimws(x))
    }

    att_choices <- function(x) {
      labs <- unique(att_label(x))
      c(sort(setdiff(labs, none_lab)), if (none_lab %in% labs) none_lab)
    }

    has_resultatt <- reactive({
      rows <- param_rows()
      !is.null(rows) && "Result Attribute" %in% names(rows) &&
        any(!is_blank(rows$`Result Attribute`))
    })

    resultatt_vals <- reactive({
      rows <- site_rows()
      if (!has_resultatt() || is.null(rows)) {
        return(character(0))
      }

      att_choices(rows$`Result Attribute`)
    })

    output$has_resultatt <- renderText({
      paste(has_resultatt())
    })
    outputOptions(output, "has_resultatt", suspendWhenHidden = FALSE)

    # keeps the user's picks when the choices change, values new to the
    # choices start out selected
    resultatt_prev <- reactiveVal(NULL)
    observe({
      vals <- resultatt_vals()
      prev <- resultatt_prev()
      sel <- if (is.null(prev)) {
        vals
      } else {
        c(intersect(input$resultatt, vals), setdiff(vals, prev))
      }

      resultatt_prev(vals)
      freezeReactiveValue(input, "resultatt")
      shinyWidgets::updatePickerInput(
        inputId = "resultatt",
        choices = vals,
        selected = sel
      )
    }) |>
      bindEvent(resultatt_vals())

    # Result attribute values passed to MassWateR for plotting/download. NULL
    # means every choice is selected, so nothing is filtered. Otherwise the
    # selected labels are mapped back to the raw values in the selected sites'
    # rows, with "None" mapping to the empty entries (NA matches NA in
    # utilMWRfilter()). Only values present after the site filter are passed
    # since MassWateR errors on any others. An empty result means no data and
    # blanks the plot.
    resultatt_sel <- reactive({
      vals <- resultatt_vals()
      if (length(vals) == 0) {
        return(NULL)
      }

      sel <- intersect(input$resultatt, vals)
      if (setequal(sel, vals)) {
        return(NULL)
      }

      raw <- unique(site_rows()$`Result Attribute`)
      raw[att_label(raw) %in% sel]
    })

    # Location groups are never passed to MassWateR. The linked selection
    # already narrows the sites to exactly the selected groups, and
    # utilMWRfilter() can't match sites with no group. This only flags
    # whether the group selection is narrowed, for the plot title.
    locgroup_filtered <- reactive({
      vals <- locgroup_vals()
      length(vals) > 0 && !setequal(intersect(input$locgroup, vals), vals)
    })

    # Grouping the Date plot by location group drops sites with no group
    # (MassWateR can't match them), so it needs at least one row from a
    # grouped site after the site and result attribute filters. Otherwise
    # MassWateR errors and the plot is left blank instead.
    has_grouped_rows <- reactive({
      rows <- site_rows()
      att <- resultatt_sel()
      if (!is.null(att)) {
        rows <- rows[rows$`Result Attribute` %in% att, ]
      }

      any(groups_of(rows$`Monitoring Location ID`) != none_lab)
    })

    byresultatt_sel <- reactive({
      isTRUE(input$byresultatt) && has_resultatt()
    })

    # Plot titles ----
    # MassWateR's own title builder (utilMWRtitle()) just checks whether an
    # argument is non-NULL to decide whether to mention it, so passing the
    # raw sidebar values straight through would always say "filtered by"
    # dates/sites/etc. even when a filter is sitting at its full, unnarrowed
    # default. These reactives compare the current selection against the
    # full/default set for each filter so the title only mentions a filter
    # when the user has actually narrowed it.
    full_date_range <- reactive({
      req(fsetls()$res, input$param)
      fsetls()$res |>
        dplyr::filter(.data$`Characteristic Name` == input$param) |>
        dplyr::pull(.data$`Activity Start Date`) |>
        range() |>
        as.Date()
    })

    date_filtered <- reactive({
      req(input$date_range)
      !isTRUE(all(as.Date(input$date_range) == full_date_range()))
    })

    sites_filtered <- reactive({
      vals <- valid_sites_calc()
      sel <- input$sites
      length(vals) > 0 && length(sel) > 0 && !setequal(sel, vals)
    })

    plot_title <- reactive({
      param <- input$param
      sites <- input$sites
      req(param)

      MassWateR::utilMWRtitle(
        param = param,
        site = if (isTRUE(sites_filtered())) sites else NULL,
        dtrng = if (isTRUE(date_filtered())) input$date_range else NULL,
        locgroup = if (isTRUE(locgroup_filtered())) input$locgroup else NULL,
        resultatt = resultatt_sel()
      )
    })

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

      freezeReactiveValue(input, "thresh")
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
        numericInput(
          ns("plot_fontsize"),
          "Font size",
          value = 12,
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
        resultatt <- resultatt_sel()

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
            resultatt = resultatt,
            confint = confint,
            bssize = input$plot_fontsize
          ) +
            ggplot2::labs(title = plot_title())
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
            resultatt = resultatt,
            confint = confint,
            bssize = input$plot_fontsize
          ) +
            ggplot2::labs(title = plot_title())
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
            resultatt = resultatt,
            byresultatt = byresultatt_sel(),
            confint = confint,
            bssize = input$plot_fontsize
          ) +
            ggplot2::labs(title = plot_title())
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
            resultatt = resultatt,
            addwater = watsel,
            maptype = mapsel,
            bssize = input$plot_fontsize
          ) +
            ggplot2::labs(title = plot_title())
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
      resultatt <- resultatt_sel()

      req(
        fsetls()$res, fsetls()$acc, param, date_range, sites,
        is.null(resultatt) || length(resultatt) > 0
      )

      anlzMWRseason(
        res = fsetls()$res,
        param = param,
        acc = fsetls()$acc,
        sit = fsetls()$sit,
        thresh = thresh,
        type = type,
        dtrng = date_range,
        site = sites,
        resultatt = resultatt,
        confint = confint,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = plot_title())
    })

    output$date_plot <- renderPlot({
      # inputs
      thresh <- if (is.null(input$thresh)) "none" else input$thresh
      param <- input$param
      date_range <- as.character(input$date_range)
      sites <- input$sites
      group <- input$group
      confint <- isTRUE(as.logical(input$confint))
      resultatt <- resultatt_sel()

      req(
        fsetls()$res, fsetls()$acc, param, date_range, sites,
        is.null(resultatt) || length(resultatt) > 0,
        group != "locgroup" || has_grouped_rows()
      )

      anlzMWRdate(
        res = fsetls()$res,
        param = param,
        acc = fsetls()$acc,
        sit = fsetls()$sit,
        thresh = thresh,
        group = group,
        dtrng = date_range,
        site = sites,
        resultatt = resultatt,
        confint = confint,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = plot_title())
    })

    output$site_plot <- renderPlot({
      # inputs
      thresh <- if (is.null(input$thresh)) "none" else input$thresh
      param <- input$param
      date_range <- as.character(input$date_range)
      sites <- input$sites
      type <- input$type
      confint <- isTRUE(as.logical(input$confint))
      resultatt <- resultatt_sel()

      req(
        fsetls()$res, fsetls()$acc, param, date_range, sites,
        is.null(resultatt) || length(resultatt) > 0
      )

      anlzMWRsite(
        res = fsetls()$res,
        param = param,
        acc = fsetls()$acc,
        sit = fsetls()$sit,
        thresh = thresh,
        type = type,
        dtrng = date_range,
        site = sites,
        resultatt = resultatt,
        byresultatt = byresultatt_sel(),
        confint = confint,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = plot_title())
    })

    output$map_plot <- renderPlot({
      # inputs
      param <- input$param
      date_range <- as.character(input$date_range)
      sites <- input$sites
      watsel <- input$watsel
      mapsel <- input$mapsel
      resultatt <- resultatt_sel()

      req(
        fsetls()$res, fsetls()$acc, fsetls()$sit, param, date_range, sites,
        is.null(resultatt) || length(resultatt) > 0
      )

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
        resultatt = resultatt,
        addwater = watsel,
        maptype = mapsel,
        bssize = 18,
        warn = FALSE
      ) +
        ggplot2::labs(title = plot_title())
    })
  })
}
