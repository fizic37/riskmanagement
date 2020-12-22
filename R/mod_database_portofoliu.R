#' database_portofoliu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_database_portofoliu_ui <- function(id){
  ns <- NS(id)
  
  shinydashboard::box( title = "Sinteza portfolio data. Click refresh to see updated data.",status = "success",width = 12,collapsible = TRUE,collapsed = TRUE,
                       DT::dataTableOutput(ns("sinteza_portofoliu")) %>% shinycssloaders::withSpinner(color = "#77547a"),br(),br(),
                       uiOutput(ns("database_portof_output")))
  
  
}
    
#' database_portofoliu Server Function
#'
#' @noRd 
mod_database_portofoliu_server <- function(input, output, session,vals){
  ns <- session$ns
  
  # Workflow is designed like this: the app is initially selected on database-portofoliu and thus portof_database
  # within this module will be first read.
  
  threshold_date_input <- as.Date("2018-12-31")
  
  portof_database <- readRDS(file = "R/reactivedata/portof_database.rds")
  
  observe({vals$portofoliu_database <-  portof_database })
  
 
  output$sinteza_portofoliu <- DT::renderDataTable({
    dt_generate_function(df = portof_database %>% dplyr::group_by(anul_de_raportare) %>%
      dplyr:: summarise(nr_contracte = dplyr::n(),garantii_sold = sum(expunere),contragarantii=sum(contragarantii),
            provizion_contabil=sum(provizion_contabil)), round_col = 2:5) })
  
  output$database_portof_output <- renderUI({
    if (all(is.null(vals$input_save_portof),is.null(input$delete_portof_database))) {
      fillRow(flex = c(1,NA),downloadButton(outputId = session$ns("down_portof_database"),label = "Download portfolio database"),
              actionButton(inputId = session$ns("delete_portof_database"),label = "Delete observations",
                                     icon = icon("minus-square"),width = "230px"))   }
    else {
      fluidRow(column(width = 4,downloadButton(outputId = session$ns("down_portof_database"),label = "Download portfolio database")),
               column(width = 4,actionButton(inputId = session$ns("delete_portof_database"),label = "Delete observations",
                                             icon = icon("minus-square"),width = "230px")),
               column(width = 4,actionButton(inputId = session$ns("refresh_portof_database"),label = "Refresh Portofoliu",
                                             icon = icon("redo"),width = "230px")))  
    } 
  })
  
  observeEvent(input$delete_portof_database,{
      
      withProgress(expr = {
      
      portof_database <-   readRDS(file = "R/reactivedata/portof_database.rds")
      dates_portof_database <-  portof_database %>% dplyr::pull(var = anul_de_raportare) %>% unique() %>% sort()
      shinyWidgets::inputSweetAlert(session = session,input = "select",inputId = session$ns("date_delete_portof"),type = "warning",
                  inputOptions = dates_portof_database[which(dates_portof_database>threshold_date_input)],
                                    btn_colors = "#f3d512",btn_labels = "OK",
                                    title = "Selecteaza data raportului pe care vrei sa-l stergi") 
      
  
      observeEvent(input$date_delete_portof,{
        saveRDS(file = "R/reactivedata/portof_database.rds",version = 3,compress = "gzip", object =  dplyr::filter(portof_database,
              anul_de_raportare != as.Date.numeric(as.numeric(input$date_delete_portof),origin = "1970-01-01")))
        
      vals$portofoliu_database <-  dplyr::filter(portof_database,
                  anul_de_raportare != as.Date.numeric(as.numeric(input$date_delete_portof),origin = "1970-01-01")) 
      
      })
      },message = "Processing for deletion")
    
    })
  
 
  
  observeEvent(input$refresh_portof_database,{
    output$sinteza_portofoliu <- DT::renderDataTable({
      dt_generate_function(df = readRDS('R/reactivedata/portof_database.rds') %>% dplyr::group_by(anul_de_raportare) %>%
                             dplyr:: summarise(nr_contracte = dplyr::n(),garantii_sold = sum(expunere),contragarantii=sum(contragarantii),
                                               provizion_contabil=sum(provizion_contabil)), round_col = 2:5) })
  })
  
  output$down_portof_database <- downloadHandler(filename = function() {"portof_database.csv"},
      content = function(file) {readr::write_csv(x = readRDS('R/reactivedata/portof_database.rds'),path = file) })
  
  
}
    
## To be copied in the UI
# mod_database_portofoliu_ui("database_portofoliu_ui_1")
    
## To be copied in the server
# callModule(mod_database_portofoliu_server, "database_portofoliu_ui_1")
 
