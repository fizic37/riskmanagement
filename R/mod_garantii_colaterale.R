#' garantii_colaterale UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_garantii_colaterale_ui <- function(id){
  ns <- NS(id)
  
  
  
  
  fluidRow(
    
    shinydashboard::box(width = 12, title = "Proceseaza fisierul de la banci",status = "success",
              collapsible = T,collapsed = F,
      shinydashboard::tabBox(width = 12,tabPanel(title = "Updated Database",icon = icon("database"),
                downloadButton(outputId = ns("down_baza_colaterale_update"),label = "Download baza colaterale update"),hr(),                                 
                DT::dataTableOutput(ns("colaterale_update"))),
          tabPanel(title = "ProcessBankFiles",icon = icon("tools"),
              uiOutput(ns("show_save_colaterale")),
              rhandsontable::rHandsontableOutput(ns("colaterale_procesat_handson"))))),
              
    
        
           
  shinydashboard::box(title = "Upload files from banks",status = "success",width = 12,collapsible = TRUE,collapsed = FALSE,
    
    fluidRow(column(width=6,
    shinybusy::add_busy_spinner(color = "#77547a", position = "bottom-right", timeout = 200),
        selectInput(inputId = ns("select_file"),label = "Select file from bank:",selected = "none",width = "300px",
                                choices = c("none",list.files("inst/extdata/colaterale")))),          
    #fileInput(inputId = ns("colateral_banci_input"),buttonLabel = "Excel only",placeholder = "no file uploaded",
       #      label = "Upload fisierul de la banci",accept = c(".xlsx",".xls"),width="300px")),
    column(width=6,uiOutput(ns("show_error_colaterale")))),
      
       DT::dataTableOutput(ns("fisier_upload_banci"))),
  
  shinydashboard::box(title = "Baza Date Charisma",status = "success",width = 12,collapsible = TRUE,collapsed = TRUE,
                      fluidRow(actionButton(inputId = ns("filter_colaterale"),
                      label = "Arata doar contractele cu ValoareAdmisaInGarantie > 0",icon = icon("filter")),br(), br(),
                      DT::dataTableOutput(ns("baza_date_colaterale"))))
  )
  }
    
#' garantii_colaterale Server Function
#'
#' @noRd 
mod_garantii_colaterale_server <- function(input, output, session){
  ns <- session$ns
 
  load("data/baza_colaterale.rda")
  load("data/baza_colaterale_update.rda")
  
  vals <- reactiveValues(baza_colaterale=baza_colaterale)
  
  nume_obligatorii_colateral_input <- c("Nr. Crt","Beneficiar","CUI","Numar contract de garantare FNGCIMM",
                                        "Tip garantie accesorie","Descriere garantie accesorie","Data Evaluare","Valoare evaluata",
                                        "Moneda","Valoare admisa in garantie","Ultima data a evaluarii",
                                        "Valoare evaluata (actualizata)","Valoare admisa in garantie de finantator (actualizata)",
                                        "Observatii")
  column_names_date <- c("Data Evaluare","Ultima data a evaluarii")
 
  colaterale_reactiva <- reactiveValues(nume_obligatorii=nume_obligatorii_colateral_input,column_names_date=column_names_date,
                                        baza_colaterale_update=baza_colaterale_update)
  
  output$link_bank_files <- downloadHandler(filename = {input$select_file},content = function(file) {file.copy(from = 
                    paste0("inst/extdata/colaterale/",input$select_file),to = file)})
  
  output$down_baza_colaterale_update <- downloadHandler(filename = function(){"baza_colaterale_update.csv"},
          content = function(file) {readr::write_csv(x = colaterale_reactiva$ baza_colaterale_update, path = file)})
  
  output$colaterale_update <- DT::renderDataTable({DT::datatable(data = colaterale_reactiva$baza_colaterale_update %>%
        dplyr::select(IdGarantie,NumeContract,Banca,Garantie,ValoareAdmisaInGarantie,DataEvaluare,
              new_ValoareAdmisaInGarantie,new_DataEvaluare) %>% 
          dplyr::mutate(dplyr::across(.cols = c("NumeContract","Banca"),~as.factor(.x))),rownames = FALSE,filter = "top",
                  caption = "Baza de date colaterale de actualizat:",options = list(scrollY=TRUE,scrollX=TRUE)) %>%
      DT::formatRound(columns = c("ValoareAdmisaInGarantie","new_ValoareAdmisaInGarantie")) })
  
  observeEvent(input$filter_colaterale,{
  if ((input$filter_colaterale %% 2) >0) {
  vals$baza_colaterale <- dplyr::filter(.data = baza_colaterale,ValoareAdmisaInGarantie!=0) 
  updateActionButton(session = session,inputId = "filter_colaterale",label = "Arata toata baza de date",icon = icon("database"))}
  
  else {vals$baza_colaterale <- baza_colaterale
  updateActionButton(session = session,inputId = "filter_colaterale",
      label = "Arata doar contractele cu ValoareAdmisaInGarantie > 0",icon = icon("filter"))  }
  })
  
  
  output$baza_date_colaterale <- DT::renderDataTable({DT::datatable(data = vals$baza_colaterale %>% 
        dplyr::select(-contains("Id"),-Evaluator) %>% dplyr::relocate(c(Banca,NumeContract),.before=Garantie) %>%
          dplyr::mutate(dplyr::across(.cols = c(Banca,NumeContract), ~as.factor(.x))),
            filter = "top",rownames = FALSE,class = "hover",
          caption = "Baza date Charisma colaterale:", options = list(scrollY=TRUE,scrollX = TRUE,pageLength=5,
            columnDefs = list(list(targets = c(2:3), searchable = FALSE),list(targets=c(0:1),width="120px"),
                  list(targets = c(6:7),width="80px")))) %>%
      DT::formatRound(columns = c(5,7),digits = 2)})
  
  
  observeEvent(input$select_file,{
    #input$colateral_banci_input,{
    
    
    #colaterale_reactiva$file_input <- input$colateral_banci_input$datapath
    colaterale_reactiva$file_input <- paste0("inst/extdata/colaterale/",input$select_file)
    
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive = colaterale_reactiva)
    
    
    observeEvent(colaterale_reactiva$all_names,{
      
      
      if (colaterale_reactiva$all_names==FALSE) {
        output$show_error_colaterale <- renderUI({
          paste("Lipseste coloana: ",colaterale_reactiva$missing_names,collapse = " ; ")      })
      }
      else if (colaterale_reactiva$all_names==TRUE) {
        
        output$show_error_colaterale <- renderUI({ fluidRow(br(), downloadLink(outputId = ns("link_bank_files"),
                      label = "Click aici pentru a downloada fisierul selectat") )     })
        
        colaterale_reactiva$fisier_prelucrat <- colaterale_reactiva$file_read_prel %>% dplyr::filter(Beneficiar!="2") %>%
          dplyr::filter(!is.na(`Nr. Crt`))
        
        
        output$fisier_upload_banci <- DT::renderDataTable({DT::datatable(class="nowrap",rownames = FALSE,# extensions = "FixedColumns",
          filter = "top", caption = "Fisierul uploadat:",    data = colaterale_reactiva$fisier_prelucrat %>% 
            dplyr::mutate(dplyr::across(.cols = "Numar contract de garantare FNGCIMM", ~as.factor(.x))),
            options = list(scrollY='350px',info=FALSE,paging=FALSE,scrollX=TRUE)) %>% 
            DT::formatRound(columns = c(8,10,12,13))         }) 
        
        colaterale_reactiva$upload_de_actualizat <- colaterale_reactiva$fisier_prelucrat %>%
          dplyr::left_join(baza_colaterale %>% dplyr::select(ValoareEvaluata,NumeContract,
                    Admisa_Charisma=ValoareAdmisaInGarantie,DataEvaluare,ValabilPanaLa,IdGarantie),
                           by = c("Numar contract de garantare FNGCIMM" = "NumeContract",
                                  "Valoare evaluata" = "ValoareEvaluata",
                                  "Data Evaluare" = "DataEvaluare")) %>%
          dplyr::filter(Admisa_Charisma >0) %>% dplyr::rename_at(.vars = "Data Evaluare",~"Data_eval_Charisma") %>%
          dplyr::mutate(new_ValoareAdmisaInGarantie = `Valoare admisa in garantie de finantator (actualizata)`,
                        new_DataEvaluare = `Ultima data a evaluarii`) %>%
          dplyr::relocate(c("Valoare admisa in garantie de finantator (actualizata)","Ultima data a evaluarii",
                            "Admisa_Charisma","Data_eval_Charisma"),
                        .after = "Numar contract de garantare FNGCIMM") %>%
          dplyr::mutate_at(.vars = c('IdGarantie','new_ValoareAdmisaInGarantie'),as.numeric) %>%
          dplyr::mutate_at(.vars = 'new_DataEvaluare',as.Date)
        
        output$colaterale_procesat <- DT::renderDataTable({DT::datatable(rownames = FALSE,class = "nowrap",
          caption = "Voi actualiza doar inregistrarile de mai jos:",
          data = colaterale_reactiva$upload_de_actualizat,extensions = 'FixedColumns',
          editable = list(target = "row", disable = list(columns = c(1,2,3,4))),
          options = list(scrollY = TRUE, scrollX = TRUE, info=FALSE,paging=FALSE,searching=FALSE,
              fixedColumns = list(leftColumns = 3, rightColumns = 4),
              columnDefs = list(list(targets=5,width="150px")))) %>%
            DT::formatRound(columns = c(15,17)) %>%
            DT::formatStyle(columns = c(17,18),color = 'gray', backgroundColor = 'yellow', fontWeight = 'bold')})
        
        
        output$colaterale_procesat_handson <- rhandsontable::renderRHandsontable({
            rhandsontable::rhandsontable(data = colaterale_reactiva$upload_de_actualizat %>% dplyr::select(-Beneficiar, -CUI),
                          readOnly = TRUE,rowHeaders = NULL,search = TRUE,height = 300) %>%
            rhandsontable::hot_col(col = 16,type = "numeric",readOnly = FALSE,format = "0,0.00") %>%
            rhandsontable::hot_col(col = c(3,5),type = "numeric",format = "0,0.00") %>%
            rhandsontable::hot_validate_numeric(cols = 16,min = 0,allowInvalid = FALSE) %>%
            rhandsontable::hot_col(col = 17,type = "date",allowInvalid = FALSE,format = "DD-MM-YYYY",readOnly = FALSE) %>%
            rhandsontable::hot_cols(fixedColumnsLeft = 6,manualColumnResize = TRUE,manualColumnMove = FALSE)
        })
        
       
        
        
        output$show_save_colaterale <- renderUI({req(colaterale_reactiva$upload_de_actualizat)
          fluidRow(column(width=6,h5("Completeaza/Verifica ultimele 2 coloane si salveaza:")),
          column(width=6, div(class="pull-right",actionButton(inputId = ns("save_colaterale"),
                          label = "Click aici pentru a salva",icon=icon("save")))),
          hr())    })
        
      }
    })
    
  })
      
      

        
    observeEvent(input$save_colaterale,{
        
        
        colaterale_reactiva$baza_colaterale_upload <- isolate(rhandsontable::hot_to_r(input$colaterale_procesat_handson)) %>%
                dplyr::select(IdGarantie,new_ValoareAdmisaInGarantie,new_DataEvaluare) %>% 
                  dplyr::left_join(y = baza_colaterale %>% dplyr::select(IdGarantie,NumeContract,Banca,Garantie,
                        ValoareAdmisaInGarantie,DataEvaluare),by = "IdGarantie")
                  
          
          check_upload <- reactive({janitor::compare_df_cols_same(baza_colaterale_update,
                    colaterale_reactiva$baza_colaterale_update, bind_method = "bind_rows",    verbose = FALSE) }) 
          
          if (any(is.na(c(colaterale_reactiva$baza_colaterale_upload$new_ValoareAdmisaInGarantie,
                          colaterale_reactiva$baza_colaterale_upload$new_DataEvaluare)))) {
            
            output$diverse <- renderPrint({any(is.na(c(colaterale_reactiva$baza_colaterale_upload$new_ValoareAdmisaInGarantie,
                                            colaterale_reactiva$baza_colaterale_upload$new_DataEvaluare))) })
            shinyWidgets::sendSweetAlert(session = session,title = "STOP",text = "Nu pot salva date lipsa. 
                  Completeaza datele lipsa in coloana new_ValoareAdmisaInGarantie sau new_DataEvaluare",
                                         type = "error",showCloseButton = TRUE)     }
          
          else if (!check_upload()){
            shinyWidgets::sendSweetAlert(session = session,title = "STOP",text = "Am intampinat o eroare si nu pot salva",
                  type = "error",showCloseButton = TRUE,
                  compare_df_cols_same(baza_colaterale_update,
                      colaterale_reactiva$baza_colaterale_update, bind_method = "bind_rows",    verbose = TRUE))  }
          
          else if (check_upload() & any(colaterale_reactiva$baza_colaterale_upload$IdGarantie %in% 
                                        colaterale_reactiva$baza_colaterale_update$IdGarantie)) {
            shinyWidgets::inputSweetAlert(session = session,inputId = "overwrite_colateral",input = "radio",
                                          inputOptions = c("Overwrite?","Renunta!"),
                                          title = "ATENTIE",type = "warning",showCloseButton = FALSE,
                                          text = "Am deja garantii actualizate in baza de date") 
            observeEvent(input$overwrite_colateral,{
              if (input$overwrite_colateral == "Renunta!"){shinyWidgets::closeSweetAlert(session=session)}
              else {
                
                baza_colaterale_update <- isolate(dplyr::bind_rows(baza_colaterale_update %>% 
                          dplyr::filter(!baza_colaterale_update$IdGarantie %in% colaterale_reactiva$baza_colaterale_upload$IdGarantie),
                                            colaterale_reactiva$baza_colaterale_update))
                usethis::use_data(baza_colaterale_update,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
                
                output$colaterale_update <- DT::renderDataTable({DT::datatable(data = baza_colaterale_update)})
              }
            })
            }
          else if (check_upload()) {
            isolate (colaterale_reactiva$baza_colaterale_update <- dplyr::bind_rows(colaterale_reactiva$baza_colaterale_upload,
                                colaterale_reactiva$baza_colaterale_update) )
            
            baza_colaterale_update <- isolate(colaterale_reactiva$baza_colaterale_update)
            usethis::use_data(baza_colaterale_update,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
            
            shinyWidgets::sendSweetAlert(session = session,title = "SUCCES", type ="success",showCloseButton = TRUE,
                                         text = "Baza de date a fost uploadata cu succes") 
            
            #output$colaterale_update <- DT::renderDataTable({DT::datatable(data = baza_colaterale_update)})
          }
    })

            
}
    
## To be copied in the UI
# mod_garantii_colaterale_ui("garantii_colaterale_ui_1")
    
## To be copied in the server
# callModule(mod_garantii_colaterale_server, "garantii_colaterale_ui_1")
 
