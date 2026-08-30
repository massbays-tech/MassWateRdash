#' qc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_underline(
      title = help_btn(ns("show_help")),
      full_screen = T,
      bslib::nav_panel(
        "DQO tables",
        bslib::navset_pill(
          bslib::nav_panel(
            "Frequency & Completeness",
            reactable::reactableOutput(ns("frecomdat_table"))
          ),
          bslib::nav_panel(
            "Accuracy",
            reactable::reactableOutput(ns("accdat_table"))
          )
        )
      ),
      bslib::nav_panel(
        "Accuracy",
        bslib::navset_pill(
          bslib::nav_panel(
            "Percent",
            reactable::reactableOutput(ns("tabaccper"))
          ),
          bslib::nav_panel(
            "Summary",
            reactable::reactableOutput(ns("tabaccsum"))
          )
        )
      ),
      bslib::nav_panel(
        "Frequency",
        bslib::navset_pill(
          bslib::nav_panel(
            "Percent",
            reactable::reactableOutput(ns("tabfreper"))
          ),
          bslib::nav_panel(
            "Summary",
            reactable::reactableOutput(ns("tabfresum"))
          )
        )
      ),
      bslib::nav_panel(
        "Completeness",
        reactable::reactableOutput(ns("tabcom"))
      ),
      bslib::nav_panel(
        "Raw Data",
        bslib::navset_pill(
          bslib::nav_panel(
            "Field Duplicates",
            reactable::reactableOutput(ns("indflddup"))
          ),
          bslib::nav_panel(
            "Lab Duplicates",
            reactable::reactableOutput(ns("indlabdup"))
          ),
          bslib::nav_panel(
            "Field Blanks",
            reactable::reactableOutput(ns("indfldblk"))
          ),
          bslib::nav_panel(
            "Lab Blanks",
            reactable::reactableOutput(ns("indlabblk"))
          ),
          bslib::nav_panel(
            "Lab Spikes / Instrument Checks",
            reactable::reactableOutput(ns("indlabins"))
          )
        )
      ),
      bslib::nav_panel(
        "Report",
        dl_btn(ns("dl_qc"), "Download quality control report")
      )
    )
  )
}

#' qc Server Functions
#'
#' @noRd
mod_qc_server <- function(id, fsetls, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Help modal ----
    mod_tab_help_server(
      "help",
      tab_value = "qc",
      active_tab = active_tab,
      force_show = reactive(input$show_help),
      title = "QC Reporting",
      body_ui = shiny::includeMarkdown(app_sys("app/www/help/qc.md"))
    )

    # dqo table frecomdat
    output$frecomdat_table <- reactable::renderReactable({
      req(fsetls()$frecom)

      frecomdat_reactable(fsetls()$frecom)
    })

    # dqo table accdat
    output$accdat_table <- reactable::renderReactable({
      req(fsetls()$acc)

      accdat_reactable(fsetls()$acc)
    })

    # frequency table percent
    output$tabfreper <- reactable::renderReactable({
      req(fsetls()$res, fsetls()$acc, fsetls()$frecom)

      tabMWRfre(
        res = fsetls()$res,
        acc = fsetls()$acc,
        frecom = fsetls()$frecom,
        type = "percent",
        warn = F
      ) |>
        flextable_to_reactable()
    })

    # frequency summary table
    output$tabfresum <- reactable::renderReactable({
      req(fsetls()$res, fsetls()$acc, fsetls()$frecom)

      tabMWRfre(
        res = fsetls()$res,
        acc = fsetls()$acc,
        frecom = fsetls()$frecom,
        type = "summary",
        warn = F
      ) |>
        flextable_to_reactable(group_by = "Type")
    })

    # accuracy table percent
    output$tabaccper <- reactable::renderReactable({
      req(fsetls()$res, fsetls()$acc, fsetls()$frecom)

      tabMWRacc(
        res = fsetls()$res,
        acc = fsetls()$acc,
        frecom = fsetls()$frecom,
        type = "percent",
        warn = F
      ) |>
        flextable_to_reactable()
    })

    # accuracy table summary
    output$tabaccsum <- reactable::renderReactable({
      req(fsetls()$res, fsetls()$acc, fsetls()$frecom)

      tabMWRacc(
        res = fsetls()$res,
        acc = fsetls()$acc,
        frecom = fsetls()$frecom,
        type = "summary",
        warn = F
      ) |>
        flextable_to_reactable(group_by = "Type")
    })

    # completeness table
    output$tabcom <- reactable::renderReactable({
      req(fsetls()$res, fsetls()$frecom)

      tabMWRcom(
        res = fsetls()$res,
        frecom = fsetls()$frecom,
        cens = fsetls()$cens,
        warn = F,
        parameterwd = 1.15
      ) |>
        flextable_to_reactable()
    })

    # individual field duplicates
    output$indflddup <- reactable::renderReactable({
      req(fsetls()$res, fsetls()$acc, fsetls()$frecom)

      tabMWRacc(
        res = fsetls()$res,
        acc = fsetls()$acc,
        frecom = fsetls()$frecom,
        type = "individual",
        accchk = "Field Duplicates",
        warn = F,
        caption = F
      ) |>
        flextable_to_reactable(group_by = "Parameter")
    })

    # individual lab duplicates
    output$indlabdup <- reactable::renderReactable({
      req(fsetls()$res, fsetls()$acc, fsetls()$frecom)

      tabMWRacc(
        res = fsetls()$res,
        acc = fsetls()$acc,
        frecom = fsetls()$frecom,
        type = "individual",
        accchk = "Lab Duplicates",
        warn = F,
        caption = F
      ) |>
        flextable_to_reactable(group_by = "Parameter")
    })

    # individual field blanks
    output$indfldblk <- reactable::renderReactable({
      req(fsetls()$res, fsetls()$acc, fsetls()$frecom)

      tabMWRacc(
        res = fsetls()$res,
        acc = fsetls()$acc,
        frecom = fsetls()$frecom,
        type = "individual",
        accchk = "Field Blanks",
        warn = F,
        caption = F
      ) |>
        flextable_to_reactable(group_by = "Parameter")
    })

    # individual lab blanks
    output$indlabblk <- reactable::renderReactable({
      req(fsetls()$res, fsetls()$acc, fsetls()$frecom)

      tabMWRacc(
        res = fsetls()$res,
        acc = fsetls()$acc,
        frecom = fsetls()$frecom,
        type = "individual",
        accchk = "Lab Blanks",
        warn = F,
        caption = F
      ) |>
        flextable_to_reactable(group_by = "Parameter")
    })

    # individual lab spikes/instrument checks
    output$indlabins <- reactable::renderReactable({
      req(fsetls()$res, fsetls()$acc, fsetls()$frecom)

      tabMWRacc(
        res = fsetls()$res,
        acc = fsetls()$acc,
        frecom = fsetls()$frecom,
        type = "individual",
        accchk = "Lab Spikes / Instrument Checks",
        warn = F,
        caption = F
      ) |>
        flextable_to_reactable(group_by = "Parameter")
    })

    # Download ----
    output$dl_qc <- downloadHandler(
      filename = function() {
        "qcreport.docx"
      },
      content = function(file) {
        qcMWRreview(
          fset = fsetls(),
          output_dir = dirname(file),
          output_file = basename(file)
        )
      }
    )
  })
}
