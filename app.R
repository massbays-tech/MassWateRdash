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
        width = 500,
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
  
  # Outlier assessment -----
  nav_panel("Outlier assessment",
    page_sidebar(
      sidebar = sidebar(
        title = "Options",
        width = 500,
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
          uiOutput("dwnldoutwrdbutt"),
          uiOutput("dwnldoutzipbutt")
        )
      )
    )
  ),
  
  # QC reporting -----
  nav_panel("QC reporting",
    navset_card_underline(
      full_screen = T,
      nav_panel(
        "DQO tables", 
        navset_pill(
          nav_panel(
            "Frequency & Completeness",
            uiOutput("frecomdat_table")
          ),
          nav_panel(
            "Accuracy",
            uiOutput("accdat_table")
          )
        )
      ),
      nav_panel(
        "Accuracy",
        navset_pill(
          nav_panel(
            "Percent",
            uiOutput("tabaccper")
          ), 
          nav_panel(
            "Summary",
            uiOutput("tabaccsum")
          )
        )
      ),
      nav_panel(
        "Frequency",
        navset_pill(
          nav_panel(
            "Percent",
            uiOutput("tabfreper")
          ),
          nav_panel(
            "Summary",
            uiOutput("tabfresum")
          )
        )
      ),
      nav_panel(
        "Completeness",
        uiOutput("tabcom")
      ),
      nav_panel(
        "Raw Data",
        navset_pill(
          nav_panel(
            "Field Duplicates",
            uiOutput("indflddup")
          ),
          nav_panel(
            "Lab Duplicates",
            uiOutput("indlabdup")
          ),
          nav_panel(
            "Field Blanks",
            uiOutput("indfldblk")
          ),
          nav_panel(
            "Lab Blanks",
            uiOutput("indlabblk")
          ),
          nav_panel(
            "Lab Spikes / Instrument Checks",
            uiOutput("indlabins")
          )
        )
      ),
      nav_panel(
        "Report",
        uiOutput("dwnldqcbutt")
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
        width = 500,
        uiOutput("prm2"),
        uiOutput("dtrng2"),
        uiOutput("sites2"),
        selectInput("thresh", "Treshold type (excludes map)", choices = c('fresh', 'marine', 'none')),
        selectInput("type2", "Plot type (season, site plots only)", choices = c("box", "jitterbox", "bar", "jitterbar", "jitter")),
        selectInput("confint2", "Show confidence (excludes map)", choices = c(F, T)),
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
        ),
        nav_panel(
          "Map",
          selectInput('watsel', 'Water feature detail', choices = c('low', 'medium', 'high', "none" = 'NULL')),
          selectInput('mapsel', 'Basemap selection', choices = c("none" = 'NULL', "OpenStreetMap", "OpenStreetMap.DE", "OpenStreetMap.France", "OpenStreetMap.HOT", "OpenTopoMap", "Esri.WorldStreetMap", "Esri.DeLorme", "Esri.WorldTopoMap", "Esri.WorldImagery", "Esri.WorldTerrain", "Esri.WorldShadedRelief", "Esri.OceanBasemap", "Esri.NatGeoWorldMap", "Esri.WorldGrayCanvas", "CartoDB.Positron", "CartoDB.PositronNoLabels", "CartoDB.PositronOnlyLabels", "CartoDB.DarkMatter", "CartoDB.DarkMatterNoLabels", "CartoDB.DarkMatterOnlyLabels", "CartoDB.Voyager", "CartoDB.VoyagerNoLabels", "CartoDB.VoyagerOnlyLabels")),
          plotOutput("map_plot")
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
      need(!is.null(fset$res), 'Waiting for input data...')
    )
    
    tosel <- sort(unique(fset$res$`Characteristic Name`))
    
    selectInput("param1", "Parameter", choices = tosel)
    
  })
  
  output$prm2 <- renderUI({
    
    # inputs
    fset <- fsetls()
    
    validate(
      need(!is.null(fset$res), 'Waiting for input data...')
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
    
    sliderInput("dtrng1", "Date range", min = tosel[1], max = tosel[2], value = tosel, width = '95%')
    
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
    
    sliderInput("dtrng2", "Date range", min = tosel[1], max = tosel[2], value = tosel, width = '95%')
    
  })
  
  output$sites2 <- renderUI({
    
    # inputs
    param2 <- input$param2
    dtrng2 <- input$dtrng2
    fset <- fsetls()
    
    req(dtrng2)

    tosel <- fset$res |> 
      dplyr::filter(`Characteristic Name` == param2) |> 
      dplyr::filter(`Activity Start Date` >= dtrng2[1] & `Activity Start Date` <= dtrng2[2]) |> 
      dplyr::pull(`Monitoring Location ID`) |> 
      unique() |> 
      sort()
    
    selectInput("sites2", "Select sites", choices = tosel, selected = tosel, selectize = T, multiple = T)
    
  })
  
  output$dwnldoutwrdbutt <- renderUI({
    
    req(input$tester | (!is.null(data_states$resdat) & !is.null(data_states$accdat))) 
    
    shinyWidgets::downloadBttn('dwnldoutwrd', 'Download outlier report: Word', style = 'simple', block = T, color = 'success')
    
  })
  
  output$dwnldoutzipbutt <- renderUI({
    
    req(input$tester | (!is.null(data_states$resdat) & !is.null(data_states$accdat)))
    
    shinyWidgets::downloadBttn('dwnldoutzip', 'Download outlier report: Zipped images', style = 'simple', block = T, color = 'success')
    
  })
  
  output$dwnldqcbutt <- renderUI({
    
    req(input$tester | (!is.null(data_states$resdat) & !is.null(data_states$accdat) & !is.null(data_states$frecomdat))) 
    
    shinyWidgets::downloadBttn('dwnldqc', 'Download quality control report', style = 'simple', block = T, color = 'success')
    
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
        resdat <- data_states$resdat
        accdat <- data_states$accdat
        frecomdat <- data_states$frecomdat
        sitdat <- data_states$sitdat
        wqxdat <- data_states$wqxdat
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
  
  # Outlier assessment -----
  output$outlier_plot <- renderPlot({
    
    # inputs
    param1 <- input$param1
    dtrng1 <- as.character(input$dtrng1)
    group1 <- input$group1
    type1 <- input$type1
    
    req(dtrng1)
    
    anlzMWRoutlier(fset = fsetls(), param = param1, group = group1, type = type1, dtrng = dtrng1, bssize = 18) + 
      ggplot2::labs(title = NULL)
    
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
  
  # download outlier report word
  output$dwnldoutwrd <- downloadHandler(
    filename = function(){'outlierreport.docx'},
    content = function(file){
      
      # inputs
      dtrng1 <- as.character(input$dtrng1)
      group1 <- input$group1
      type1 <- input$type1

      anlzMWRoutlierall(fset = fsetls(), group = group1, type = type1, dtrng = dtrng1, format = 'word', 
                        output_dir = dirname(file), 
                        output_file = basename(file))
      
    }
  )
  
  # download outlier report zip
  output$dwnldoutzip <- downloadHandler(
    filename = function(){'outlierreport.zip'},
    content = function(file){
      
      # inputs
      dtrng1 <- as.character(input$dtrng1)
      group1 <- input$group1
      type1 <- input$type1
      
      anlzMWRoutlierall(fset = fsetls(), group = group1, type = type1, dtrng = dtrng1, format = 'zip', 
                        output_dir = dirname(file), 
                        output_file = basename(file))
      
    }
  )
  
  # QC reporting -----
  
  # dqo table frecomdat
  output$frecomdat_table <- renderUI({
    
    req(fsetls()$frecom)
    
    frecomdat_tab(fsetls()$frecom, dqofontsize, padding, wd)
    
  })
  
  # dqo table accdat
  output$accdat_table <- renderUI({
    
    req(fsetls()$acc)
    
    accdat_tab(fsetls()$acc, dqofontsize, padding, wd)
    
  })
  
  # frequency table percent
  output$tabfreper <- renderUI({
    
    req(fsetls()$res, fsetls()$acc, fsetls()$frecom)
    
    tabMWRfre(res = fsetls()$res, acc = fsetls()$acc, frecom = fsetls()$frecom, type = 'percent', warn = F) |>
      thmsum(wd = wd) |> 
      flextable::htmltools_value()
    
  })
  
  # frequency summary table
  output$tabfresum <- renderUI({
    
    req(fsetls()$res, fsetls()$acc, fsetls()$frecom)
    
    tabMWRfre(res = fsetls()$res, acc = fsetls()$acc, frecom = fsetls()$frecom, type = 'summary', warn = F) |>
      thmsum(wd = wd) |> 
      flextable::htmltools_value()
    
  })
  
  # accuracy table percent
  output$tabaccper <- renderUI({
    
    req(fsetls()$res, fsetls()$acc, fsetls()$frecom)
    
    tabMWRacc(res = fsetls()$res, acc = fsetls()$acc, frecom = fsetls()$frecom, type = 'percent', warn = F) |>
      thmsum(wd = wd) |> 
      flextable::htmltools_value()
    
  })
  
  # accuracy table summary
  output$tabaccsum <- renderUI({
    
    req(fsetls()$res, fsetls()$acc, fsetls()$frecom)
    
    tabMWRacc(res = fsetls()$res, acc = fsetls()$acc, frecom = fsetls()$frecom, type = 'summary', warn = F) |>
      thmsum(wd = wd) |> 
      flextable::htmltools_value()
    
  })
  
  # completeness table
  output$tabcom <- renderUI({
    
    req(fsetls()$res, fsetls()$frecom)
    
    out <- tabMWRcom(res = fsetls()$res, frecom = fsetls()$frecom, warn = F, parameterwd = 1.15)
    out <- out |> 
      flextable::width(width = (wd - 3.15) / (flextable::ncol_keys(out) - 2), j = 2:(flextable::ncol_keys(out) - 1)) |>
      flextable::htmltools_value()
    
    return(out)
    
  })
  
  # individual field duplicates
  output$indflddup <- renderUI({
    
    req(fsetls()$res, fsetls()$acc, fsetls()$frecom)
    
    tabMWRacc(res = fsetls()$res, acc = fsetls()$acc, frecom = fsetls()$frecom, type = 'individual', accchk = 'Field Duplicates', warn = F, caption = F) |> 
      thmsum(wd = wd) |> 
      flextable::htmltools_value()
    
  })
  
  # individual lab duplicates
  output$indlabdup <- renderUI({
    
    req(fsetls()$res, fsetls()$acc, fsetls()$frecom)
    
    tabMWRacc(res = fsetls()$res, acc = fsetls()$acc, frecom = fsetls()$frecom, type = 'individual', accchk = 'Lab Duplicates', warn = F, caption = F) |> 
      thmsum(wd = wd) |> 
      flextable::htmltools_value()
    
  })
  
  # individual field blanks
  output$indfldblk <- renderUI({
    
    req(fsetls()$res, fsetls()$acc, fsetls()$frecom)
    
    tabMWRacc(res = fsetls()$res, acc = fsetls()$acc, frecom = fsetls()$frecom, type = 'individual', accchk = 'Field Blanks', warn = F, caption = F) |> 
      thmsum(wd = wd) |> 
      flextable::htmltools_value()
    
  })
  
  # individual lab blanks
  output$indlabblk <- renderUI({
    
    req(fsetls()$res, fsetls()$acc, fsetls()$frecom)
    
    tabMWRacc(res = fsetls()$res, acc = fsetls()$acc, frecom = fsetls()$frecom, type = 'individual', accchk = 'Lab Blanks', warn = F, caption = F) |> 
      thmsum(wd = wd) |> 
      flextable::htmltools_value()
    
  })
  
  # individual lab spikes/instrument checks
  output$indlabins <- renderUI({
    
    req(fsetls()$res, fsetls()$acc, fsetls()$frecom)
    
    tabMWRacc(res = fsetls()$res, acc = fsetls()$acc, frecom = fsetls()$frecom, type = 'individual', accchk = 'Lab Spikes / Instrument Checks', warn = F, caption = F) |> 
      thmsum(wd = wd) |> 
      flextable::htmltools_value()
    
  })
  
  # download qc report word
  output$dwnldqc <- downloadHandler(
    filename = function(){'qcreport.docx'},
    content = function(file){
      
      qcMWRreview(fset = fsetls(), 
                  output_dir = dirname(file), 
                  output_file = basename(file))
      
    }
  )
  
  # Visualize ----
  output$season_plot <- renderPlot({
    
    # inputs
    thresh <- input$thresh
    param2 <- input$param2
    dtrng2 <- as.character(input$dtrng2)
    sites2 <- input$sites2
    type2 <- input$type2
    confint2 <- as.logical(input$confint2)
    
    req(dtrng2)

    anlzMWRseason(fset = fsetls(), param = param2, thresh = thresh, type = type2, dtrng = dtrng2, site = sites2, confint = confint2, bssize = 18) + 
      ggplot2::labs(title = NULL)
    
  })
  
  output$date_plot <- renderPlot({
    
    # inputs
    thresh <- input$thresh
    param2 <- input$param2
    dtrng2 <- as.character(input$dtrng2)
    sites2 <- input$sites2
    group2 <- input$group2
    confint2 <- as.logical(input$confint2)
    
    req(dtrng2)
    
    anlzMWRdate(fset = fsetls(), param = param2, thresh = thresh, group = group2, dtrng = dtrng2, site = sites2, confint = confint2, bssize = 18) + 
      ggplot2::labs(title = NULL)
    
  })
  
  output$site_plot <- renderPlot({
    
    # inputs
    thresh <- input$thresh
    param2 <- input$param2
    dtrng2 <- as.character(input$dtrng2)
    sites2 <- input$sites2
    type2 <- input$type2
    confint2 <- as.logical(input$confint2)
    
    req(dtrng2)
    
    anlzMWRsite(fset = fsetls(), param = param2, thresh = thresh, type = type2, dtrng = dtrng2, site = sites2, confint = confint2, bssize = 18) + 
      ggplot2::labs(title = NULL)
    
  })
  
  output$map_plot <- renderPlot({
    
    # inputs
    param2 <- input$param2
    dtrng2 <- as.character(input$dtrng2)
    sites2 <- input$sites2
    watsel <- input$watsel
    mapsel <- input$mapsel
    
    req(dtrng2)
    
    if(watsel == 'NULL')
      watsel <- NULL
    if(mapsel == 'NULL')
      mapsel <- NULL
    
    anlzMWRmap(fset = fsetls(), param = param2, dtrng = dtrng2, site = sites2, addwater = watsel, maptype = mapsel, bssize = 18) + 
      ggplot2::labs(title = NULL)
    
  })
  
}

shinyApp(ui = ui, server = server)