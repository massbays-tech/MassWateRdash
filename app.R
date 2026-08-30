pkgload::load_all(".", quiet = TRUE)

addResourcePath(
  prefix = "toimg",
  directoryPath = "www"
)

# ui -----
ui <- bslib::page_navbar(
  id = "navbar",
  header = tags$head(
    tab_help_deps(),
    tags$script(HTML(
      "$(document).on('shown.bs.modal', function(e) {
        $(e.target).find('.rhandsontable').each(function() {
          var ht = HTMLWidgets.getInstance(this);
          if (ht && ht.hot) { ht.hot.render(); }
        });
      });"
    )),
    tags$style(HTML(
      ".rhandsontable .htCore thead th { white-space: nowrap; }
       .modal-dialog.modal-xl { max-width: 90vw; }
       .value-box-value { font-size: 1.5rem !important; }
       .fill-height { height: calc(100vh - 58px); overflow: hidden; }
       .card-scroll .card-body { overflow-y: auto; }
       .shiny-download-link:hover { filter: brightness(0.88); }
       #show_format_modal:hover, #open_plot_download:hover { filter: brightness(0.88); }
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
       .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background-color: #00A4CF !important; }"
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
    class = 'fill-height',
    title = "Overview",
    value = 'overview',
    bslib::navset_card_underline(
      full_screen = TRUE,
      height = '100%',
      bslib::nav_panel(
        title = '',
        class = 'card-scroll',
        shiny::includeMarkdown('www/overview.md')
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

  # Modules ----
  fsetls <- mod_upload_server("upload", active_tab)
  mod_outlier_server("outlier", fsetls, active_tab)
  mod_qc_server("qc", fsetls, active_tab)
  mod_wqx_server("wqx", fsetls, active_tab)
  mod_visualize_server("visualize", fsetls, active_tab)

  # Show/hide nav items ---
  observe({
    chk <- length(unlist(fsetls()))

    if (chk < 6) {
      bslib::nav_hide("navbar", "outlier")
      bslib::nav_hide("navbar", "qc")
      bslib::nav_hide("navbar", "wqx")
      bslib::nav_hide("navbar", "visualize")
    } else {
      bslib::nav_show("navbar", "outlier")
      bslib::nav_show("navbar", "qc")
      bslib::nav_show("navbar", "wqx")
      bslib::nav_show("navbar", "visualize")
    }
  }) |>
    bindEvent(fsetls())
}

shinyApp(ui = ui, server = server)
