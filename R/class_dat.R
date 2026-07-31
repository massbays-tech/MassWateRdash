resClass <- R6::R6Class(
  "resClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL, dat = NULL, del_dat = NULL, msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)

accClass <- R6::R6Class(
  "accClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL, dat = NULL, del_dat = NULL, msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)

frecomClass <- R6::R6Class(
  "frecomClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL, dat = NULL, del_dat = NULL, msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)

sitClass <- R6::R6Class(
  "sitClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL, dat = NULL, del_dat = NULL, msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)

wqxClass <- R6::R6Class(
  "wqxClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL, dat = NULL, del_dat = NULL, msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)

censClass <- R6::R6Class(
  "censClass",
  public = list(
    raw_dat = NULL,
    dat = NULL,
    del_dat = NULL,
    msg = "",
    initialize = function(
      raw_dat = NULL, dat = NULL, del_dat = NULL, msg = ""
    ) {
      self$raw_dat <- raw_dat
      self$dat <- dat
      self$del_dat <- del_dat
      self$msg <- msg
    }
  )
)
