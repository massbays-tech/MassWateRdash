# dqo table theme
thmdqo <- function(x, dqofontsize, padding) {
  flextable::colformat_double(x, na_str = "-") |>
    flextable::colformat_char(na_str = "-") |>
    flextable::border_inner() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(align = "left", j = 1, part = "all") |>
    flextable::fontsize(size = dqofontsize, part = "all") |>
    flextable::padding(padding = padding, part = "all")
}

# dqo summary table theme
thmsum <- function(x, wd) {
  if (!is.null(x)) {
    flextable::width(x, width = wd / flextable::ncol_keys(x))
  }
}

# frecomdat table
frecomdat_tab <- function(frecomdat, dqofontsize, padding, wd) {
  frecomdat |>
    dplyr::mutate_if(is.numeric, as.character) |>
    dplyr::mutate_all(function(x) ifelse(is.na(x), "-", x)) |>
    dplyr::arrange(.data$Parameter, .locale = "en") |>
    flextable::flextable() |>
    thmdqo(dqofontsize = dqofontsize, padding = padding) |>
    flextable::width(width = wd / ncol(frecomdat)) |>
    flextable::add_header_row(
      value = c("", "Frequency %", ""), colwidths = c(1, 5, 1)
    ) |>
    flextable::set_caption("Frequency and Completeness") |>
    flextable::htmltools_value()
}

# accdat table
accdat_tab <- function(accdat, dqofontsize, padding, wd) {
  out <- accdat |>
    dplyr::mutate_if(is.numeric, as.character) |>
    dplyr::mutate_all(function(x) ifelse(is.na(x), "-", x)) |>
    dplyr::arrange(.data$Parameter, .locale = "en") |>
    flextable::flextable() |>
    thmdqo(dqofontsize = dqofontsize, padding = padding) |>
    flextable::width(width = 1, j = 1)

  out |>
    flextable::width(
      width = (wd - 1) / (flextable::ncol_keys(out) - 1),
      j = 2:flextable::ncol_keys(out)
    ) |>
    flextable::set_caption("Accuracy") |>
    flextable::htmltools_value()
}
