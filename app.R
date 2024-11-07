library(shiny)
library(bslib)
library(MassWateR)

ui <- page_navbar(
  title = "MassWateR Dashboard",
  
  # Add logo
  nav_item(
    tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px;")
  ),
  
  nav_panel("Upload & Validate",
            page_sidebar(
              sidebar = sidebar(
                title = "Upload Data Files",
                width = 400,
                card(
                  fileInput("results", "Upload Results Data (.xlsx)", accept = ".xlsx"),
                  fileInput("accuracy", "Upload DQO Accuracy Data (.xlsx)", accept = ".xlsx"),
                  fileInput("frecom", "Upload DQO Frequency & Completeness Data (.xlsx)", accept = ".xlsx"),
                  fileInput("sites", "Upload Site Data (.xlsx)", accept = ".xlsx"),
                  fileInput("wqx", "Upload WQX Meta Data (.xlsx)", accept = ".xlsx")
                )
              ),
              
              layout_columns(
                fill = FALSE,
                value_box(
                  title = "Results Data",
                  value = textOutput("results_status")
                ),
                value_box(
                  title = "Accuracy Data",
                  value = textOutput("accuracy_status")
                ),
                value_box(
                  title = "Frequency & Completeness",
                  value = textOutput("frecom_status")
                ),
                value_box(
                  title = "Sites Data",
                  value = textOutput("sites_status")
                ),
                value_box(
                  title = "WQX Meta Data",
                  value = textOutput("wqx_status")
                )
              ),
              
              card(
                card_header("Data Validation Messages"),
                verbatimTextOutput("validation_messages", placeholder = FALSE)
              )
            )
  ),
  
  nav_panel("Visualize",
            card(
              card_header("Title"),
              p("Description")
            )
  ),
  
  nav_panel("QC reporting",
            card(
              card_header("Title"),
              p("Description")
            )
  ),
  
  nav_panel("WQX output",
            card(
              card_header("Title"),
              p("Description")
            )
  ),
  
  nav_spacer(),
  
  nav_item(
    tags$a(
      href = "https://github.com/massbays-tech/MassWateR",
      target = "_blank",
      "Source Code"
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store validation messages and data states
  validation_log <- reactiveVal("")
  data_states <- reactiveValues(
    results = NULL,
    accuracy = NULL,
    frecom = NULL,
    sites = NULL,
    wqx = NULL
  )
  
  # Function to capture and log messages
  capture_messages <- function(expr) {
    # Create a text connection to capture output
    temp <- textConnection("messages", "w", local = TRUE)
    sink(temp, type = "message")
    on.exit({
      sink(type = "message")
      close(temp)
    })
    
    result <- expr
    
    # Get the captured messages
    if(exists("messages")) {
      current_log <- validation_log()
      new_msgs <- paste(messages, collapse = "\n")
      validation_log(paste0(current_log, if(nchar(current_log) > 0) "\n", new_msgs))
    }
    
    return(result)
  }
  
  # Observers for each data upload
  observeEvent(input$results, {
    req(input$results)
    validation_log("")  # Clear previous messages
    data_states$results <- tryCatch({
      capture_messages({
        readMWRresults(input$results$datapath)
      })
    }, error = function(e) {
      validation_log(paste0("Error in results: ", e$message))
      NULL
    })
  })
  
  observeEvent(input$accuracy, {
    req(input$accuracy)
    validation_log("")  # Clear previous messages
    data_states$accuracy <- tryCatch({
      capture_messages({
        readMWRacc(input$accuracy$datapath)
      })
    }, error = function(e) {
      validation_log(paste0("Error in accuracy: ", e$message))
      NULL
    })
  })
  
  observeEvent(input$frecom, {
    req(input$frecom)
    validation_log("")  # Clear previous messages
    data_states$frecom <- tryCatch({
      capture_messages({
        readMWRfrecom(input$frecom$datapath)
      })
    }, error = function(e) {
      validation_log(paste0("Error in frecom: ", e$message))
      NULL
    })
  })
  
  observeEvent(input$sites, {
    req(input$sites)
    validation_log("")  # Clear previous messages
    data_states$sites <- tryCatch({
      capture_messages({
        readMWRsites(input$sites$datapath)
      })
    }, error = function(e) {
      validation_log(paste0("Error in sites: ", e$message))
      NULL
    })
  })
  
  observeEvent(input$wqx, {
    req(input$wqx)
    validation_log("")  # Clear previous messages
    data_states$wqx <- tryCatch({
      capture_messages({
        readMWRwqx(input$wqx$datapath)
      })
    }, error = function(e) {
      validation_log(paste0("Error in wqx: ", e$message))
      NULL
    })
  })
  
  # Status outputs
  output$results_status <- renderText({
    if(is.null(input$results)) return("No file uploaded")
    if(is.null(data_states$results)) "Error loading" else "Data loaded"
  })
  
  output$accuracy_status <- renderText({
    if(is.null(input$accuracy)) return("No file uploaded")
    if(is.null(data_states$accuracy)) "Error loading" else "Data loaded"
  })
  
  output$frecom_status <- renderText({
    if(is.null(input$frecom)) return("No file uploaded")
    if(is.null(data_states$frecom)) "Error loading" else "Data loaded"
  })
  
  output$sites_status <- renderText({
    if(is.null(input$sites)) return("No file uploaded")
    if(is.null(data_states$sites)) "Error loading" else "Data loaded"
  })
  
  output$wqx_status <- renderText({
    if(is.null(input$wqx)) return("No file uploaded")
    if(is.null(data_states$wqx)) "Error loading" else "Data loaded"
  })
  
  # Output validation messages
  output$validation_messages <- renderText({
    validation_log()
  })
}

shinyApp(ui = ui, server = server)