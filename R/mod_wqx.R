#' wqx UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_wqx_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_underline(
      full_screen = TRUE,
      bslib::nav_panel(
        "Projects",
        reactable::reactableOutput(ns("tabwqxprojects"))
      ),
      bslib::nav_panel(
        "Locations",
        reactable::reactableOutput(ns("tabwqxlocations"))
      ),
      bslib::nav_panel(
        "Results",
        reactable::reactableOutput(ns("tabwqxresults"))
      ),
      bslib::nav_panel(
        "Workbook",
        dl_btn(ns("dl_wqx"), "Download WQX workbook")
      )
    )
  )
}

#' wqx Server Functions
#'
#' @noRd
mod_wqx_server <- function(id, fsetls, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Help modal ----
    mod_tab_help_server(
      "help",
      tab_value = "wqx",
      active_tab = active_tab,
      title = "WQX Output",
      body_ui = tagList(p("Help text coming soon."))
    )

    # list output
    tabwqx <- reactive({
      req(fsetls()$res, fsetls()$acc, fsetls()$sit, fsetls()$wqx)

      tabMWRwqx(fset = fsetls(), listout = TRUE, warn = FALSE)
    })

    # projects table
    output$tabwqxprojects <- reactable::renderReactable({
      req(tabwqx())

      reactable::reactable(
        tabwqx()$Projects,
        defaultColDef = reactable::colDef(
          resizable = TRUE
        ),
        filterable = TRUE
      )
    })

    # locations table
    output$tabwqxlocations <- reactable::renderReactable({
      req(tabwqx())

      reactable::reactable(
        tabwqx()$Locations,
        defaultColDef = reactable::colDef(
          resizable = TRUE
        ),
        filterable = TRUE
      )
    })

    # results table
    output$tabwqxresults <- reactable::renderReactable({
      req(tabwqx())

      reactable::reactable(
        tabwqx()$Results,
        defaultColDef = reactable::colDef(
          resizable = TRUE
        ),
        filterable = TRUE
      )
    })

    # download wqx workbook
    output$dl_wqx <- downloadHandler(
      filename = function() {
        "wqxtab.xlsx"
      },
      content = function(file) {
        tabMWRwqx(
          fset = fsetls(),
          output_dir = dirname(file),
          output_file = basename(file)
        )
      }
    )
  })
}
