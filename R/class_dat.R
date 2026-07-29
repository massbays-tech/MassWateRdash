resdatClass <- R6::R6Class(
  "resdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    initialize = function(raw_dat = NULL, dat = NULL, del_dat = NULL) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
    }
  )
)

accdatClass <- R6::R6Class(
  "accdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    initialize = function(raw_dat = NULL, dat = NULL, del_dat = NULL) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
    }
  )
)

frecomdatClass <- R6::R6Class(
  "frecomdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    initialize = function(raw_dat = NULL, dat = NULL, del_dat = NULL) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
    }
  )
)

sitdatClass <- R6::R6Class(
  "sitdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    initialize = function(raw_dat = NULL, dat = NULL, del_dat = NULL) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
    }
  )
)

wqxdatClass <- R6::R6Class(
  "wqxdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    initialize = function(raw_dat = NULL, dat = NULL, del_dat = NULL) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
    }
  )
)

censdatClass <- R6::R6Class(
  "censdatClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    initialize = function(raw_dat = NULL, dat = NULL, del_dat = NULL) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
    }
  )
)
