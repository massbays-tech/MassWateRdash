validationLog <- R6::R6Class(
  "validationLog",
  public = list(
    msg = "",
    result = NULL,
    edit_dat = "",

    catch_msg = function(expr) {
      # Create a text connection to capture output
      temp <- textConnection("messages", "w", local = TRUE)
      sink(temp, type = "message")
      on.exit({
        sink(type = "message")
        close(temp)
      })

      self$result <- expr

      # Get the captured messages
      if (exists("messages")) {
        current_log <- if (nchar(self$msg) > 0) paste0(self$msg, "\n") else ""

        new_msgs <- paste(messages, collapse = "\n")
        new_msgs <- gsub("\\\033..;|\\\033.", "", new_msgs)

        self$msg <- paste0(current_log, new_msgs)
      }

      invisible(self)
    },

    initialize = function(msg = "") {
      self$msg <- msg
    }
  )
)

resdatClass <- R6::R6Class(
  "resdatClass",
  inherit = validationLog,
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
  inherit = validationLog,
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
  inherit = validationLog,
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
  inherit = validationLog,
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
  inherit = validationLog,
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
  inherit = validationLog,
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
