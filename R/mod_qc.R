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
      full_screen = T,
      bslib::nav_panel(
        "DQO tables",
        bslib::navset_pill(
          bslib::nav_panel(
            "Frequency & Completeness",
            uiOutput(ns("frecomdat_table"))
          ),
          bslib::nav_panel(
            "Accuracy",
            uiOutput(ns("accdat_table"))
          )
        )
      ),
      bslib::nav_panel(
        "Accuracy",
        bslib::navset_pill(
          bslib::nav_panel(
            "Percent",
            uiOutput(ns("tabaccper"))
          ),
          bslib::nav_panel(
            "Summary",
            uiOutput(ns("tabaccsum"))
          )
        )
      ),
      bslib::nav_panel(
        "Frequency",
        bslib::navset_pill(
          bslib::nav_panel(
            "Percent",
            uiOutput(ns("tabfreper"))
          ),
          bslib::nav_panel(
            "Summary",
            uiOutput(ns("tabfresum"))
          )
        )
      ),
      bslib::nav_panel(
        "Completeness",
        uiOutput(ns("tabcom"))
      ),
      bslib::nav_panel(
        "Raw Data",
        bslib::navset_pill(
          bslib::nav_panel(
            "Field Duplicates",
            uiOutput(ns("indflddup"))
          ),
          bslib::nav_panel(
            "Lab Duplicates",
            uiOutput(ns("indlabdup"))
          ),
          bslib::nav_panel(
            "Field Blanks",
            uiOutput(ns("indfldblk"))
          ),
          bslib::nav_panel(
            "Lab Blanks",
            uiOutput(ns("indlabblk"))
          ),
          bslib::nav_panel(
            "Lab Spikes / Instrument Checks",
            uiOutput(ns("indlabins"))
          )
        )
      ),
      bslib::nav_panel(
        "Report",
        uiOutput(ns("dwnldqcbutt"))
      )
    )
  )
}

#' qc Server Functions
#'
#' @noRd
mod_qc_server <- function(id, fsetls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # dqo table frecomdat
    output$frecomdat_table <- renderUI({
      req(fsetls$frecom())

      frecomdat_tab(fsetls$frecom(), dqofontsize, padding, wd)
    })

    # dqo table accdat
    output$accdat_table <- renderUI({
      req(fsetls$acc())

      accdat_tab(fsetls$acc(), dqofontsize, padding, wd)
    })

    # frequency table percent
    output$tabfreper <- renderUI({
      req(fsetls$res(), fsetls$acc(), fsetls$frecom())

      tabMWRfre(
        res = fsetls$res(),
        acc = fsetls$acc(),
        frecom = fsetls$frecom(),
        type = "percent",
        warn = F
      ) |>
        thmsum(wd = wd) |>
        flextable::htmltools_value()
    })

    # frequency summary table
    output$tabfresum <- renderUI({
      req(fsetls$res(), fsetls$acc(), fsetls$frecom())

      tabMWRfre(
        res = fsetls$res(),
        acc = fsetls$acc(),
        frecom = fsetls$frecom(),
        type = "summary",
        warn = F
      ) |>
        thmsum(wd = wd) |>
        flextable::htmltools_value()
    })

    # accuracy table percent
    output$tabaccper <- renderUI({
      req(fsetls$res(), fsetls$acc(), fsetls$frecom())

      tabMWRacc(
        res = fsetls$res(),
        acc = fsetls$acc(),
        frecom = fsetls$frecom(),
        type = "percent",
        warn = F
      ) |>
        thmsum(wd = wd) |>
        flextable::htmltools_value()
    })

    # accuracy table summary
    output$tabaccsum <- renderUI({
      req(fsetls$res(), fsetls$acc(), fsetls$frecom())

      tabMWRacc(
        res = fsetls$res(),
        acc = fsetls$acc(),
        frecom = fsetls$frecom(),
        type = "summary",
        warn = F
      ) |>
        thmsum(wd = wd) |>
        flextable::htmltools_value()
    })

    # completeness table
    output$tabcom <- renderUI({
      req(fsetls$res(), fsetls$frecom())

      out <- tabMWRcom(
        res = fsetls$res(),
        frecom = fsetls$frecom(),
        cens = fsetls$cens(),
        warn = F,
        parameterwd = 1.15
      )
      out <- out |>
        flextable::width(
          width = (wd - 3.15) / (flextable::ncol_keys(out) - 2),
          j = 2:(flextable::ncol_keys(out) - 1)
        ) |>
        flextable::htmltools_value()

      return(out)
    })

    # individual field duplicates
    output$indflddup <- renderUI({
      req(fsetls$res(), fsetls$acc(), fsetls$frecom())

      tabMWRacc(
        res = fsetls$res(),
        acc = fsetls$acc(),
        frecom = fsetls$frecom(),
        type = "individual",
        accchk = "Field Duplicates",
        warn = F,
        caption = F
      ) |>
        thmsum(wd = wd) |>
        flextable::htmltools_value()
    })

    # individual lab duplicates
    output$indlabdup <- renderUI({
      req(fsetls$res(), fsetls$acc(), fsetls$frecom())

      tabMWRacc(
        res = fsetls$res(),
        acc = fsetls$acc(),
        frecom = fsetls$frecom(),
        type = "individual",
        accchk = "Lab Duplicates",
        warn = F,
        caption = F
      ) |>
        thmsum(wd = wd) |>
        flextable::htmltools_value()
    })

    # individual field blanks
    output$indfldblk <- renderUI({
      req(fsetls$res(), fsetls$acc(), fsetls$frecom())

      tabMWRacc(
        res = fsetls$res(),
        acc = fsetls$acc(),
        frecom = fsetls$frecom(),
        type = "individual",
        accchk = "Field Blanks",
        warn = F,
        caption = F
      ) |>
        thmsum(wd = wd) |>
        flextable::htmltools_value()
    })

    # individual lab blanks
    output$indlabblk <- renderUI({
      req(fsetls$res(), fsetls$acc(), fsetls$frecom())

      tabMWRacc(
        res = fsetls$res(),
        acc = fsetls$acc(),
        frecom = fsetls$frecom(),
        type = "individual",
        accchk = "Lab Blanks",
        warn = F,
        caption = F
      ) |>
        thmsum(wd = wd) |>
        flextable::htmltools_value()
    })

    # individual lab spikes/instrument checks
    output$indlabins <- renderUI({
      req(fsetls$res(), fsetls$acc(), fsetls$frecom())

      tabMWRacc(
        res = fsetls$res(),
        acc = fsetls$acc(),
        frecom = fsetls$frecom(),
        type = "individual",
        accchk = "Lab Spikes / Instrument Checks",
        warn = F,
        caption = F
      ) |>
        thmsum(wd = wd) |>
        flextable::htmltools_value()
    })

    # download qc report word
    output$dwnldqc <- downloadHandler(
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
