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
  
  shinydashboard::box( title = "Sinteza portfolio data. Click refresh to see updated data.",
                        status = "success",width = 12,collapsible = TRUE,collapsed = F,
                       
                       DT::dataTableOutput(ns("sinteza_portofoliu")),
                       tags$script(src = "portofoliu_buttons.js"),
                       tags$script(paste0("portofoliu_module_js('", ns(''), "')")),
                       
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
  
  
  load("data/view1_portofoliu.rda")
  
  vals_portofoliu <- reactiveValues(view1_portofoliu = view1_portofoliu)
  
  
  # Key observer. Everytime it updates I update actions and render teh main table.
  # Note I do not save it, as it will be saved from the beginning. I only save it inside vals_cip$baza_date_cip observer
  observeEvent(vals_portofoliu$view1_portofoliu,{
    
    vals_portofoliu$unique_dates <-  vals_portofoliu$view1_portofoliu$anul_de_raportare
    
    vals_portofoliu$actions <- purrr::map_chr(vals_portofoliu$unique_dates, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-sm download_btn" data-toggle="tooltip" data-placement="top" title="Download" id = ', id_, ' style="margin: 0"><i class="fa fa-download"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )  })
    output$sinteza_portofoliu <- DT::renderDataTable({ DT::datatable(rownames = F, escape = F,
      data = cbind(tibble::tibble(" " = vals_portofoliu$actions),vals_portofoliu$view1_portofoliu),
        options = list(dom = "tp",pageLength=5)) %>% 
          DT::formatRound(columns = 3:6, digits = 0) })
    
    
  })
  
  # Key observer. Every time i update vals_portofoliu$portof_database, I save the new database, recalculate view1 and save view1
  observeEvent(vals_portofoliu$portof_database,{
    vals_portofoliu$view1_portofoliu <- vals_portofoliu$portof_database %>% dplyr::group_by(anul_de_raportare) %>%
      dplyr:: summarise(nr_contracte = dplyr::n(),garantii_sold = sum(expunere),contragarantii=sum(contragarantii),
                        provizion_contabil=sum(provizion_contabil)) %>% dplyr::arrange(desc(anul_de_raportare))
    
    portof_database <- isolate(vals_portofoliu$portof_database)
    
    usethis::use_data(portof_database,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    
    view1_portofoliu <- isolate(vals_portofoliu$view1_portofoliu)
    
    usethis::use_data(view1_portofoliu,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    
  })
 
  observeEvent(input$data_raport_to_download,{
    
    showModal(modalDialog(title = h3("ATENTIE!",style = "color: #ffa500;"), size = "l",
                          h3(paste0("Esti sigur ca vrei sa downloadez portofoliul la data de ",
                                    input$data_raport_to_download, " ?"), style = "color: #77547a"),  footer = 
                            tagList(shinyWidgets::downloadBttn(outputId = session$ns("confirm_download"),label = "Download",
                                                               color = "success", size = "md"),
                                    shinyWidgets::actionBttn(inputId = session$ns("cancel_download"),label = "Cancel",
                                                             icon = icon("window-close"),color = "danger",size = "md")
                            )))
    
    output$confirm_download <- downloadHandler(filename = function(){paste0("portof_database_",input$data_raport_to_download,".csv")},
                                                content = function(file){
                  load("data/portof_database.rda")
          readr::write_csv(x = portof_database %>% dplyr::filter(anul_de_raportare == 
                        as.Date.character(input$data_raport_to_download)), path = file) })
    
    removeModal(session = session)
  })
  
  # Remove modal on cancel download button
  observeEvent(input$cancel_download,{
    removeModal(session = session)  })
  
  observeEvent(input$data_raport_to_delete,{
    
    showModal(modalDialog(title = h3("ATENTIE!",style = "color: #ffa500;"), size = "l",
                          h3(paste0("Esti sigur ca vrei sa stergi portofoliul la data de ",
                                    input$data_raport_to_delete, " ?"), style = "color: #77547a"),  footer = 
                            tagList(shinyWidgets::actionBttn(inputId = session$ns("confirm_delete"),label = "Confirm",
                                                             icon = icon("minus-square"),color = "success",size = "md"),
                                    shinyWidgets::actionBttn(inputId = session$ns("cancel_delete"),label = "Cancel",
                                                             icon = icon("window-close"),color = "danger",size = "md")
                            )))
  }) 
  
  # Remove modal on cancel delete button
  observeEvent(input$cancel_delete,{
    removeModal(session = session)  })
  
  
  observeEvent(input$confirm_delete,{
    
    load("data/portof_database.rda")
    vals_portofoliu$portof_database <- portof_database %>% dplyr::filter(anul_de_raportare != 
                                              as.Date.character(input$data_raport_to_delete))
    removeModal(session = session)
    
    
  })
  
  
  
     
    
  
  
  
 
  
  
}
    
## To be copied in the UI
# mod_database_portofoliu_ui("database_portofoliu_ui_1")
    
## To be copied in the server
# callModule(mod_database_portofoliu_server, "database_portofoliu_ui_1")
 
