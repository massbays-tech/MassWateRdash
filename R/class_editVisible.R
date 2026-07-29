editVisible <- R6::R6Class(
  "editVisible",
  public = list(
    resdat = FALSE,
    accdat = FALSE,
    frecomdat = FALSE,
    sitdat = FALSE,
    wqxdat = FALSE,
    censdat = FALSE,
    initialize = function(
      resdat = FALSE,
      accdat = FALSE,
      frecomdat = FALSE,
      sitdat = FALSE,
      wqxdat = FALSE,
      censdat = FALSE
    ) {
      self$resdat <- resdat
      self$accdat <- accdat
      self$frecomdat <- frecomdat
      self$sitdat <- sitdat
      self$wqxdat <- wqxdat
      self$censdat <- censdat
    }
  )
)
