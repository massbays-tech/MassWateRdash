#' Tab help modal - browser storage glue
#'
#' @description Include once in the app's UI header. Provides the JS glue
#' that lets `mod_tab_help_server()` persist "don't show again" per tab in
#' the browser's `localStorage`, so a dismissal survives page reloads and
#' future sessions (a plain reactive value would only last the session).
#'
#' @noRd
tab_help_deps <- function() {
  tags$script(HTML(
    "
    Shiny.addCustomMessageHandler('tabHelpDismiss', function(key) {
      try { localStorage.setItem(key, 'true'); } catch (e) {}
    });
    Shiny.addCustomMessageHandler('tabHelpCheck', function(msg) {
      var dismissed = false;
      try { dismissed = localStorage.getItem(msg.key) === 'true'; } catch (e) {}
      Shiny.setInputValue(msg.inputId, dismissed, {priority: 'event'});
    });
    "
  ))
}

#' Tab help modal server
#'
#' @description Shows a one-time help modal for a tab the first time that
#' tab is opened in a session. The modal's "Don't show again" button
#' persists the dismissal in the browser's `localStorage` so it doesn't
#' reappear on future visits either. Content is a placeholder for now - fill
#' in `body_ui` per tab later.
#'
#' @param id Namespace id for the module. Should be unique per tab, e.g.
#' `"help"` called from within `mod_outlier_server()`.
#' @param tab_value String. The `nav_panel` `value` this help modal belongs
#' to (e.g. `"outlier"`), matching the value used in `app.R`.
#' @param active_tab Reactive. Returns the currently selected navbar tab
#' value, e.g. `reactive(input$navbar)` from the top-level server.
#' @param title String. Modal title.
#' @param body_ui A tagList/HTML/character. Modal body content.
#'
#' @noRd
mod_tab_help_server <- function(id, tab_value, active_tab, title, body_ui) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    storage_key <- paste0("masswaterdash_help_dismissed_", tab_value)
    shown_this_session <- reactiveVal(FALSE)

    observe({
      session$sendCustomMessage(
        "tabHelpCheck",
        list(key = storage_key, inputId = ns("dismissed"))
      )
    })

    observe({
      req(active_tab() == tab_value)
      req(!isTRUE(shown_this_session()))
      req(!isTRUE(input$dismissed))

      shown_this_session(TRUE)

      showModal(
        modalDialog(
          title = title,
          body_ui,
          footer = tagList(
            actionButton(ns("dont_show_again"), "Don't show again"),
            modalButton("Close")
          ),
          easyClose = TRUE
        )
      )
    }) |>
      bindEvent(active_tab(), input$dismissed, ignoreNULL = FALSE)

    observe({
      session$sendCustomMessage("tabHelpDismiss", storage_key)
      removeModal()
    }) |>
      bindEvent(input$dont_show_again)
  })
}
