resdatClass <- R6::R6Class(
  "resdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL,
      dat = NULL,
      del_dat = NULL,
      msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)

accdatClass <- R6::R6Class(
  "accdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL,
      dat = NULL,
      del_dat = NULL,
      msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)

frecomdatClass <- R6::R6Class(
  "frecomdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL,
      dat = NULL,
      del_dat = NULL,
      msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)

sitdatClass <- R6::R6Class(
  "sitdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL,
      dat = NULL,
      del_dat = NULL,
      msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)

wqxdatClass <- R6::R6Class(
  "wqxdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL,
      dat = NULL,
      del_dat = NULL,
      msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)

censdatClass <- R6::R6Class(
  "censdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL,
      dat = NULL,
      del_dat = NULL,
      msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)
