source('R/global.R')

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
        shinyWidgets::materialSwitch('tester', "Test mode", FALSE),
        fileInput("resdat", "Upload Results Data (.xlsx)", accept = ".xlsx"),
        fileInput("accdat", "Upload DQO Accuracy Data (.xlsx)", accept = ".xlsx"),
        fileInput("frecomdat", "Upload DQO Frequency & Completeness Data (.xlsx)", accept = ".xlsx"),
        fileInput("sitdat", "Upload Site Data (.xlsx)", accept = ".xlsx"),
        fileInput("wqxdat", "Upload WQX Meta Data (.xlsx)", accept = ".xlsx")
      ),
      
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Results Data",
          value = htmlOutput("resdat_status")
        ),
        value_box(
          title = "Accuracy Data",
          value = htmlOutput("accdat_status")
        ),
        value_box(
          title = "Frequency & Completeness Data",
          value = htmlOutput("frecomdat_status")
        ),
        value_box(
          title = "Sites Data",
          value = htmlOutput("sitdat_status")
        ),
        value_box(
          title = "WQX Data",
          value = htmlOutput("wqxdat_status")
        )
      ),
      
      card(
        card_header("Data Validation Messages"),
        verbatimTextOutput("validation_messages", placeholder = FALSE)
      )
      
    )
            
  ),
  
  # QC reporting -----
  nav_panel("QC reporting and outlier assessment",
      
    navset_card_underline(
      full_screen = F,
      
      nav_panel(
        "QC report",
        NULL
      ),
      
      nav_panel(
        "Outliers",
        page_sidebar(
          
          sidebar = sidebar(
            title = "Options",
            width = 400,
            uiOutput("prm1"),
            uiOutput("dtrng1"),
            selectInput("group1", "Group by", choices = c("month", "week", "site")),
            selectInput("type1", "Plot type", choices = c("box", "jitterbox", "jitter"))
          ),
          navset_card_underline(
            full_screen = T,
            nav_panel(
              "Plot",
              plotOutput("outlier_plot")
            ),
            nav_panel(
              "Table",
              reactable::reactableOutput("outlier_table")
            ),
            nav_panel(
              "Report",
              NULL
            )
          )
        )
      )
      
    )
          
  ),
  
  # WQX output -----
  nav_panel("WQX output",
            card(
              card_header("Title"),
              p("Description")
            )
  ),
  
  # Visualize -----
  nav_panel("Visualize",
            
    page_sidebar(
      sidebar = sidebar(
        title = "Plot options",
        width = 400,
        uiOutput("prm2"),
        uiOutput("dtrng2"),
        selectInput("thresh", "Treshold type", choices = c('fresh', 'marine', 'none')),
        selectInput("type2", "Plot type (season, site plots only)", choices = c("box", "jitterbox", "bar", "jitterbar", "jitter")),
        selectInput("confint2", "Show confidence", choices = c(F, T)),
        selectInput("group2", "Plot grouping (date plot only)", choices = c("site", "locgroup", "all"))
      ),
      
      navset_card_underline(
        full_screen = T,
        nav_panel(
          "Season",
          plotOutput("season_plot")
        ),
        nav_panel(
          "Date",
          plotOutput("date_plot")
        ),
        nav_panel(
          "Site",
          plotOutput("site_plot")
        )
      )
      
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
  
  # reactive UI -----
  output$prm1 <- renderUI({
    
    # inputs
    fset <- fsetls()
    
    validate(
      need(!is.na(fset$res), 'Waiting for input data...')
    )
    
    tosel <- sort(unique(fset$res$`Characteristic Name`))
    
    selectInput("param1", "Parameter", choices = tosel)
    
  })
  
  output$prm2 <- renderUI({
    
    # inputs
    fset <- fsetls()
    
    validate(
      need(!is.na(fset$res), 'Waiting for input data...')
    )
    
    tosel <- sort(unique(fset$res$`Characteristic Name`))
    
    selectInput("param2", "Parameter", choices = tosel)
    
  })
  
  output$dtrng1 <- renderUI({
    
    # inputs
    param1 <- input$param1
    fset <- fsetls()
    
    req(param1)
    
    tosel <- fset$res |> 
      dplyr::filter(`Characteristic Name` == param1) |> 
      dplyr::pull(`Activity Start Date`) |> 
      range() |> 
      as.Date()
    
    sliderInput("dtrng1", "Date range", min = tosel[1], max = tosel[2], value = tosel)
    
  })
  
  output$dtrng2 <- renderUI({
    
    # inputs
    param2 <- input$param2
    fset <- fsetls()
    
    req(param2)
    
    tosel <- fset$res |> 
      dplyr::filter(`Characteristic Name` == param2) |> 
      dplyr::pull(`Activity Start Date`) |> 
      range() |> 
      as.Date()
    
    sliderInput("dtrng2", "Date range", min = tosel[1], max = tosel[2], value = tosel)
    
  })
  
  # upload & validate -----
  # Reactive values to store validation messages and data states
  validation_log <<- reactiveVal("")
  data_states <<- reactiveValues(
    resdat = NULL,
    accdat = NULL,
    frecomdat = NULL,
    sitdat = NULL,
    wqxdat = NULL
  )
  
  # Observers for each data upload
  observeEvent(input$resdat, {
    fl_upload(input$resdat, readMWRresults, "resdat")
  })
  
  observeEvent(input$accdat, {
    fl_upload(input$accdat, readMWRacc, "accdat")
  })
  
  observeEvent(input$frecomdat, {
    fl_upload(input$frecomdat, readMWRfrecom, "frecomdat")
  })
  
  observeEvent(input$sitdat, {
    fl_upload(input$sitdat, readMWRsites, "sitdat")
  })
  
  observeEvent(input$wqxdat, {
   fl_upload(input$wqxdat, readMWRwqx, "wqxdat")
  })
  
  # Status outputs
  output$resdat_status <- renderUI({
    fl_status(input$tester, input$resdat, data_states$resdat)
  })
  
  output$accdat_status <- renderUI({
    fl_status(input$tester, input$accdat, data_states$accdat)
  })
  
  output$frecomdat_status <- renderUI({
    fl_status(input$tester, input$frecomdat, data_states$frecomdat)
  })
  
  output$sitdat_status <- renderUI({
    fl_status(input$tester, input$sitdat, data_states$sitdat)
  })
  
  output$wqxdat_status <- renderUI({
    fl_status(input$tester, input$wqxdat, data_states$wqxdat)
  })
  
  # Output validation messages
  output$validation_messages <- renderText({
    validation_log()
  })
  
  # data inputs
  fsetls <- reactive({
    
    if(!input$tester){
      if(is.null(data_states$resdat))
        resdat <- NA
      if(is.null(data_states$accdat))
        accdat <- NA
      if(is.null(data_states$frecomdat))
        frecomdat <- NA
      if(is.null(data_states$sitdat))
        sitdat <- NA
      if(is.null(data_states$wqxdat))
        wqxdat <- NA
    }
    
    if(input$tester == T){
      resdat <- readMWRresults(system.file("extdata", "ExampleResults.xlsx", package = "MassWateR"), runchk = F)
      accdat <- readMWRacc(system.file("extdata", "ExampleDQOAccuracy.xlsx", package = "MassWateR"), runchk = F)
      frecomdat <- readMWRfrecom(system.file("extdata", "ExampleDQOFrequencyCompleteness.xlsx", package = "MassWateR"), runchk = F)
      sitdat <- readMWRsites(system.file("extdata", "ExampleSites.xlsx", package = "MassWateR"), runchk = F)
      wqxdat <- readMWRwqx(system.file("extdata", "ExampleWQX.xlsx", package = "MassWateR"), runchk = F) 
    }
    
    out <- list(
      res = resdat,
      acc = accdat,
      frecom = frecomdat,
      sit = sitdat,
      wqx = wqxdat
    )

    return(out)
    
  })
  
  # QC reporting -----
  output$outlier_plot <- renderPlot({
    
    # inputs
    param1 <- input$param1
    dtrng1 <- as.character(input$dtrng1)
    group1 <- input$group1
    type1 <- input$type1
    
    req(dtrng1)
    
    anlzMWRoutlier(fset = fsetls(), param = param1, group = group1, type = type1, dtrng = dtrng1, bssize = 18)
    
  })
  
  output$outlier_table <- reactable::renderReactable({
    
    # inputs
    param1 <- input$param1
    dtrng1 <- as.character(input$dtrng1)
    group1 <- input$group1
    type1 <- input$type1
    
    req(dtrng1)
    
    tab <- anlzMWRoutlier(fset = fsetls(), param = param1, group = group1, dtrng = dtrng1, outliers = T)
    
    out <- reactable::reactable(
      tab,
      defaultColDef = reactable::colDef(
        footerStyle = list(fontWeight = "bold"),
        resizable = TRUE
      ),
      filterable = T
    )
    
    return(out)
                              
  })
  
  # Visualize ----
  output$season_plot <- renderPlot({
    
    # inputs
    thresh <- input$thresh
    param2 <- input$param2
    dtrng2 <- as.character(input$dtrng2)
    type2 <- input$type2
    confint2 <- as.logical(input$confint2)
    
    req(dtrng2)

    anlzMWRseason(fset = fsetls(), param = param2, thresh = thresh, type = type2, dtrng = dtrng2, confint = confint2, bssize = 18)
    
  })
  
  output$date_plot <- renderPlot({
    
    # inputs
    thresh <- input$thresh
    param2 <- input$param2
    dtrng2 <- as.character(input$dtrng2)
    group2 <- input$group2
    confint2 <- as.logical(input$confint2)
    
    req(dtrng2)
    
    anlzMWRdate(fset = fsetls(), param = param2, thresh = thresh, group = group2, dtrng = dtrng2, confint = confint2, bssize = 18)
    
  })
  
  output$site_plot <- renderPlot({
    
    # inputs
    thresh <- input$thresh
    param2 <- input$param2
    dtrng2 <- as.character(input$dtrng2)
    type2 <- input$type2
    confint2 <- as.logical(input$confint2)
    
    req(dtrng2)
    
    anlzMWRsite(fset = fsetls(), param = param2, thresh = thresh, type = type2, dtrng = dtrng2, confint = confint2, bssize = 18)
    
  })
  
}

shinyApp(ui = ui, server = server)