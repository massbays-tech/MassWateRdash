library(shiny)
library(bslib)
library(MassWateR)

# ui -----
ui <- page_navbar(
  title = "MassWateR Dashboard",
  
  # Add logo
  nav_item(
    tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px;")
  ),
  

  # upload & validate -----
  nav_panel("Upload & Validate",
            page_sidebar(
              sidebar = sidebar(
                title = "Upload Data Files",
                width = 400,
                card(
                  fileInput("resdat", "Upload Results Data (.xlsx)", accept = ".xlsx"),
                  fileInput("accdat", "Upload DQO Accuracy Data (.xlsx)", accept = ".xlsx"),
                  fileInput("frecomdat", "Upload DQO Frequency & Completeness Data (.xlsx)", accept = ".xlsx"),
                  fileInput("sitdat", "Upload Site Data (.xlsx)", accept = ".xlsx"),
                  fileInput("wqxdat", "Upload WQX Meta Data (.xlsx)", accept = ".xlsx")
                )
              ),
              
              layout_columns(
                fill = FALSE,
                value_box(
                  title = "Results Data",
                  value = textOutput("resdat_status")
                ),
                value_box(
                  title = "Accuracy Data",
                  value = textOutput("accdat_status")
                ),
                value_box(
                  title = "Frequency & Completeness Data",
                  value = textOutput("frecomdat_status")
                ),
                value_box(
                  title = "Sites Data",
                  value = textOutput("sitdat_status")
                ),
                value_box(
                  title = "WQX Data",
                  value = textOutput("wqxdat_status")
                )
              ),
              
              card(
                card_header("Data Validation Messages"),
                verbatimTextOutput("validation_messages", placeholder = FALSE)
              )
            )
  ),
  
  # Visualize -----
  nav_panel("Visualize",
            
            # By season -----
            card(
              card_header("By season"),
              plotOutput("season_plot")
            )
  ),
  
  # QC reporting -----
  nav_panel("QC reporting",
            card(
              card_header("Title"),
              p("Description")
            )
  ),
  
  # WQX output -----
  nav_panel("WQX output",
            card(
              card_header("Title"),
              p("Description")
            )
  ),
  
  nav_spacer(),
  
  nav_item(
    tags$a(
      href = "https://github.com/massbays-tech/MassWateRdash",
      target = "_blank",
      "Source Code"
    )
  )
)

# server -----
server <- function(input, output, session) {
  
  # Reactive values to store validation messages and data states
  validation_log <- reactiveVal("")
  data_states <- reactiveValues(
    resdat = NULL,
    accdat = NULL,
    frecomdat = NULL,
    sitdat = NULL,
    wqxdat = NULL
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
  observeEvent(input$resdat, {
    req(input$resdat)
    validation_log("")  # Clear previous messages
    data_states$resdat <- tryCatch({
      capture_messages({
        resdat <<- readMWRresults(input$resdat$datapath)
      })
    }, error = function(e) {
      validation_log(paste0("Error in results: ", e$message))
      NULL
    })
  })
  
  observeEvent(input$accdat, {
    req(input$accdat)
    validation_log("")  # Clear previous messages
    data_states$accdat <- tryCatch({
      capture_messages({
        accdat <<- readMWRacc(input$accdat$datapath)
      })
    }, error = function(e) {
      validation_log(paste0("Error in accuracy data: ", e$message))
      NULL
    })
  })
  
  observeEvent(input$frecomdat, {
    req(input$frecomdat)
    validation_log("")  # Clear previous messages
    data_states$frecomdat <- tryCatch({
      capture_messages({
        frecomdat <<- readMWRfrecom(input$frecomdat$datapath)
      })
    }, error = function(e) {
      validation_log(paste0("Error in frequency completeness data: ", e$message))
      NULL
    })
  })
  
  observeEvent(input$sitdat, {
    req(input$sitdat)
    validation_log("")  # Clear previous messages
    data_states$sitdat <- tryCatch({
      capture_messages({
        sitdat <<- readMWRsites(input$sitdat$datapath)
      })
    }, error = function(e) {
      validation_log(paste0("Error in sites: ", e$message))
      NULL
    })
  })
  
  observeEvent(input$wqxdat, {
    req(input$wqxdat)
    validation_log("")  # Clear previous messages
    data_states$wqxdat <- tryCatch({
      capture_messages({
        wqxdat <<- readMWRwqx(input$wqxdat$datapath)
      })
    }, error = function(e) {
      validation_log(paste0("Error in WQX data: ", e$message))
      NULL
    })
  })
  
  # Status outputs
  output$resdat_status <- renderText({
    if(is.null(input$resdat)) return("No file uploaded")
    if(is.null(data_states$resdat)) "Error loading" else "Data loaded"
  })
  
  output$accdat_status <- renderText({
    if(is.null(input$accdat)) return("No file uploaded")
    if(is.null(data_states$accdat)) "Error loading" else "Data loaded"
  })
  
  output$frecomdat_status <- renderText({
    if(is.null(input$frecomdat)) return("No file uploaded")
    if(is.null(data_states$frecomdat)) "Error loading" else "Data loaded"
  })
  
  output$sitdat_status <- renderText({
    if(is.null(input$sitdat)) return("No file uploaded")
    if(is.null(data_states$sitdat)) "Error loading" else "Data loaded"
  })
  
  output$wqxdat_status <- renderText({
    if(is.null(input$wqxdat)) return("No file uploaded")
    if(is.null(data_states$wqxdat)) "Error loading" else "Data loaded"
  })
  
  # Output validation messages
  output$validation_messages <- renderText({
    validation_log()
  })
  
  # data inputs
  fsetls <- reactive({
    
    if(is.null(data_states$resdat))
      res <- NA
    if(is.null(data_states$accdat))
      acc <- NA
    if(is.null(data_states$frecomdat))
      frecom <- NA
    if(is.null(data_states$sitdat))
      sit <- NA
    if(is.null(data_states$wqxdat))
      wqx <- NA

    list(
      res = resdat,
      acc = accdat,
      frecom = frecomdat,
      sit = sitdat,
      wqx = wqxdat
    )

  })
  
  output$season_plot <- renderPlot({
    anlzMWRseason(fset = fsetls(), param = 'TSS', thresh = 'fresh')
  })
  
}

shinyApp(ui = ui, server = server)