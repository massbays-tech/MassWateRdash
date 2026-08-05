#' Create dropdown menu
#'
#' @description `dropdown()` creates a dropdown widget.
#'
#' @param id String. Widget id.
#' @param label String. Widget heading/label.
#' @param choices List. Dropdown choices.
#' @param choice_names List. Display names for choices. Default `NULL`.
#' @param sorted Boolean. Whether to sort the choices. Default `TRUE`.
#' @param decreasing Boolean. Whether to sort choices in descending order.
#' Default `FALSE`.
#' @param multiple Boolean. Whether to allow multiple selections. Default
#' `TRUE`.
#' @param max_options Integer. Maximum number of selections. Default `NULL`.
#'
#' @return A dropdown widget.
#'
#' @noRd
dropdown <- function(
  id, label, choices, choice_names = NULL, sorted = TRUE,
  decreasing = FALSE, multiple = TRUE, max_options = NULL
) {
  if (!is.null(choice_names)) {
    names(choices) <- choice_names
  }

  choices <- choices[!duplicated(choices)]

  if (sorted && is.null(choice_names)) {
    choices <- sort(choices, decreasing = decreasing)
  } else if (sorted) {
    choices <- choices[order(names(choices), decreasing = decreasing)]
  }

  selected <- choices[1]
  allow_actions <- FALSE
  if (multiple && is.null(max_options)) {
    selected <- choices
    allow_actions <- TRUE
  }

  shinyWidgets::pickerInput(
    id,
    label = label,
    choices = choices,
    selected = selected,
    options = list(
      `actions-box` = allow_actions,
      `live-search` = TRUE,
      `selected-text-format` = "count > 1",
      `max-options` = max_options,
      container = "body"
    ),
    multiple = multiple
  )
}

#' Create download button
#'
#' @description `dl_btn()` creates a download button.
#'
#' @param id String. Button id.
#' @param label String. Button heading/label.
#' @param block Boolean. If `TRUE`, button is full width.
#' @param size String. Button size. Acceptable values: `xs`, `sm`, `md`, `lg`.
#' Default `md`.
#'
#' @return A download button
#'
#' @noRd
dl_btn <- function(id, label, block = TRUE, size = "md") {
  if (!size %in% c("xs", "sm", "md", "lg")) {
    size <- "md"
  }

  btn <- shinyWidgets::downloadBttn(
    id,
    label = label,
    style = "simple",
    block = block,
    size = size
  )
  htmltools::tagAppendAttributes(
    btn,
    style = "background-color: #64C147 !important; border-color: #64C147 !important; color: white !important;"
  )
}
