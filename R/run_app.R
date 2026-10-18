#' Run the MassWateR Dashboard app
#'
#' @description Builds and launches the MassWateR Dashboard Shiny app. Works
#' whether the package is loaded via `pkgload::load_all()` (local development,
#' Docker) or a real install (`remotes::install_github()` then `library()`),
#' since app assets are resolved with `app_sys()` rather than relative paths.
#'
#' @param ... Arguments passed on to `shiny::shinyApp()`.
#'
#' @return A shiny app object.
#'
#' @export
#'
#' @importFrom shiny addResourcePath img shinyApp tags
run_app <- function(...) {
  addResourcePath(
    prefix = "toimg",
    directoryPath = app_sys("app/www")
  )

  # ui -----
  ui <- bslib::page_navbar(
    id = "navbar",
    header = tags$head(
      tab_help_deps(),
      shinyjs::useShinyjs(),
      tags$style(HTML(
        ".modal-dialog.modal-xl { max-width: 90vw; }
         .modal-title { flex-grow: 1; }
         .value-box-value { font-size: 1.5rem !important; }
         .fill-height { height: calc(100vh - 58px); overflow: hidden; }
         .card-scroll .card-body { overflow-y: auto; }
         .shiny-download-link:hover { filter: brightness(0.88); }
         #show_format_modal:hover, #open_plot_download:hover { filter: brightness(0.88); }
         #open_text_size { color: rgba(0, 0, 0, 0.65) !important; }
         #open_text_size:hover { color: #00A4CF !important; }
         :root { --bs-primary: #00A4CF; --bs-primary-rgb: 0, 164, 207; }
         .progress-bar { background-color: #00A4CF !important; }
         .nav-pills .nav-link.active { background-color: #00A4CF !important; }
         .bootstrap-select > .dropdown-toggle { background-color: #fff !important; border: 1px solid #6c757d !important; color: #212529 !important; box-shadow: none !important; padding: 0.375rem 0.75rem !important; line-height: 1.5 !important; font-size: 1rem !important; }
         .bootstrap-select > .dropdown-toggle:hover { background-color: #fff !important; border-color: #6c757d !important; box-shadow: none !important; }
         .bootstrap-select > .dropdown-toggle:focus, .bootstrap-select.show > .dropdown-toggle { background-color: #fff !important; border-color: #86b7fe !important; box-shadow: 0 0 0 0.25rem rgba(13,110,253,.25) !important; }
         .bootstrap-select .filter-option-inner-inner { font-size: 0.875rem !important; }
         .bootstrap-select .dropdown-menu .dropdown-item { font-size: 0.875rem !important; }
         .bootstrap-select .dropdown-menu.inner li.selected a,
         .bootstrap-select .dropdown-menu.inner li.selected a:hover,
         .bootstrap-select .dropdown-menu .dropdown-item.active,
         .bootstrap-select .dropdown-menu .dropdown-item:active { background-color: #00A4CF !important; color: white !important; }
         .bs-actionsbox .btn-group { display: flex !important; gap: 6px; }
         .bs-actionsbox .btn { background-color: #00A4CF !important; border-color: #00A4CF !important; color: white !important; border-radius: 4px !important; flex: 1; }
         .shiny-notification-message { background-color: #00A4CF !important; color: white !important; border-color: #00A4CF !important; }
         .irs--shiny .irs-bar { background: #00A4CF !important; border-top-color: #00A4CF !important; border-bottom-color: #00A4CF !important; }
         .irs--shiny .irs-bar--single { border-left-color: #00A4CF !important; }
         .irs--shiny .irs-handle { border-color: #00A4CF !important; background-color: #00A4CF !important; }
         .irs--shiny .irs-handle > i:first-child { background-color: #00A4CF !important; }
         .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover { background-color: #00A4CF !important; }
         .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background-color: #00A4CF !important; }

         /* sidebar scrollbar mod for easier selection */
         .bslib-sidebar-layout .bslib-sidebar-resize-handle .resize-indicator {
           right: -6px !important;
         }
         .bslib-sidebar-layout.sidebar-right .bslib-sidebar-resize-handle .resize-indicator {
           right: auto !important;
           left: -6px !important;
         }
         .bslib-sidebar-layout .bslib-sidebar-resize-handle::before {
           left: calc(100% + 2px) !important;
           width: 6px !important;
         }
         .bslib-sidebar-layout .bslib-sidebar-resize-handle.handle-active::before {
           left: calc(100% + 2px) !important;
           width: 18px !important;
         }
         .bslib-sidebar-layout.sidebar-right .bslib-sidebar-resize-handle::before {
           left: auto !important;
           right: calc(100% + 2px) !important;
           width: 6px !important;
         }
         .bslib-sidebar-layout.sidebar-right .bslib-sidebar-resize-handle.handle-active::before {
           left: auto !important;
           right: calc(100% + 2px) !important;
           width: 18px !important;
         }
         /* the actual draggable hit target is much wider than its visible
            indicator and sits on top of (z-index 1001) the native scrollbar
            track, capturing every click meant for the scrollbar. Shrink it
            and shift it just past the sidebar's outer edge so it no longer
            overlaps the scrollbar's pixels. */
         .bslib-sidebar-layout .bslib-sidebar-resize-handle {
           width: 8px !important;
           transform: translateX(28px);
         }
         .bslib-sidebar-layout.sidebar-right .bslib-sidebar-resize-handle {
           transform: translateX(-28px);
         }

         .bslib-sidebar-layout > .sidebar {
           scrollbar-width: auto;
           scrollbar-color: rgba(0, 0, 0, 0.35) transparent;
         }
         .bslib-sidebar-layout > .sidebar::-webkit-scrollbar {
           width: 14px;
         }
         .bslib-sidebar-layout > .sidebar::-webkit-scrollbar-track {
           background: transparent;
         }
         .bslib-sidebar-layout > .sidebar::-webkit-scrollbar-thumb {
           background-color: rgba(0, 0, 0, 0.35);
           border-radius: 7px;
           border: 3px solid transparent;
           background-clip: content-box;
         }
         .bslib-sidebar-layout > .sidebar::-webkit-scrollbar-thumb:hover {
           background-color: rgba(0, 0, 0, 0.55);
         }"
      ))
    ),
    footer = tagList(
      tags$div(
        id = "loading-indicator",
        style = "display: none; position: fixed; bottom: 15px; right: 15px; z-index: 9999;
                 background: rgba(0,0,0,0.6); color: white; padding: 5px 12px;
                 border-radius: 4px; font-size: 0.85em;",
        bsicons::bs_icon("arrow-repeat"),
        " Loading..."
      ),
      tags$style(
        "html.shiny-busy #loading-indicator { display: block !important; }"
      )
    ),
    title = span(
      img(src = "toimg/logo.png", height = "40px", style = "margin-right: 10px;"),
      "MassWateR Dashboard"
    ),

    bslib::nav_panel(
      class = "fill-height",
      title = "Overview",
      value = "overview",
      bslib::navset_card_underline(
        full_screen = TRUE,
        height = "100%",
        bslib::nav_panel(
          title = "",
          class = "card-scroll",
          shiny::includeMarkdown(app_sys("app/www/overview.md"))
        )
      )
    ),

    # Tabs ----
    bslib::nav_panel(
      "1 Upload & Validate",
      value = "upload",
      mod_upload_ui("upload")
    ),
    bslib::nav_panel(
      "2 Outlier assessment",
      value = "outlier",
      mod_outlier_ui("outlier")
    ),
    bslib::nav_panel(
      "3 QC reporting",
      value = "qc",
      mod_qc_ui("qc")
    ),
    bslib::nav_panel(
      "4 WQX output",
      value = "wqx",
      mod_wqx_ui("wqx")
    ),
    bslib::nav_panel(
      "5 Visualize",
      value = "visualize",
      mod_visualize_ui("visualize")
    ),

    bslib::nav_spacer(),

    bslib::nav_item(
      actionButton(
        "open_text_size",
        label = NULL,
        icon = icon("gear"),
        class = "btn btn-link",
        title = "Text size settings"
      )
    ),

    bslib::nav_item(
      tags$a(
        href = "https://github.com/massbays-tech/MassWateRdash",
        target = "_blank",
        "Source Code"
      )
    )
  )

  # server -----
  server <- function(input, output, session) {
    # Active tab ----
    active_tab <- reactive(input$navbar)

    # Text size ----
    observe({
      current <- if (is.null(input$text_size)) 100 else input$text_size

      showModal(modalDialog(
        title = "Text size",
        size = "s",
        sliderInput(
          "text_size",
          NULL,
          min = 50,
          max = 150,
          value = current,
          step = 10,
          post = "%",
          width = "100%"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }) |>
      bindEvent(input$open_text_size)

    observe({
      req(input$text_size)

      shinyjs::runjs(
        paste0("document.documentElement.style.fontSize = '", input$text_size, "%';")
      )
    }) |>
      bindEvent(input$text_size)

    # Modules ----
    fsetls <- mod_upload_server("upload", active_tab)
    mod_outlier_server("outlier", fsetls, active_tab)
    mod_qc_server("qc", fsetls, active_tab)
    mod_wqx_server("wqx", fsetls, active_tab)
    mod_visualize_server("visualize", fsetls, active_tab)

    # Show/hide nav items ---
    observe({
      fls <- fsetls()

      has_res <- !is.null(fls$res)
      has_acc <- !is.null(fls$acc)
      has_frecom <- !is.null(fls$frecom)
      has_sit <- !is.null(fls$sit)
      has_wqx <- !is.null(fls$wqx)

      # censdat is optional and not a requirement for the qc tab
      show <- list(
        outlier = has_res & has_acc,
        qc = has_res & has_acc & has_frecom,
        wqx = has_res & has_acc & has_sit & has_wqx,
        visualize = has_res & has_acc & has_sit
      )

      for (tab in names(show)) {
        if (show[[tab]]) {
          bslib::nav_show("navbar", tab)
        } else {
          bslib::nav_hide("navbar", tab)
        }
      }
    }) |>
      bindEvent(fsetls())
  }

  shinyApp(ui = ui, server = server, ...)
}
