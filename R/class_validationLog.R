validationLog <- R6::R6Class(
  "validationLog",
  public = list(
    msg = "",
    result = NULL,
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
