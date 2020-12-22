#' database_grupuri UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_database_grupuri_ui <- function(id){
  ns <- NS(id)
  
  
  
  shinydashboard::box(title = "Prelucrarea grupurilor",width = 12,collapsible = FALSE,
                      footer = "Se uploadeaza fisierul din Charisma-Rapoarte adapative - Grupuri IMMExpuneri/Plati",status = "success",
  fluidRow(shinybusy::add_busy_spinner(color = "#77547a", position = "bottom-right", timeout = 200),
            column(width=6,fileInput(inputId = ns("grupuri_upload"),label = "Upload grupuri",width = "300px",
                        accept = c(".xlsx",".xls"), buttonLabel = "Excel only",placeholder = "no file uploaded")),
            column(width = 6, uiOutput(ns("show_grup_date"))),
            column(width=12,DT::dataTableOutput(ns("grupuri_selectate"))),
            column(width = 12, DT::dataTableOutput(ns("grupuri_neconstituite")))
            )  )
}
    
#' database_grupuri Server Function
#'
#' @noRd 
mod_database_grupuri_server <- function(input, output, session){
  ns <- session$ns
  
  
  nume_obligatorii_grupuri <- c("GrupId","NrGrup","Grup","Beneficiar","CUI","Nrcontract")
  
 
  
  grupuri_reactive <- reactiveValues(nume_obligatorii=nume_obligatorii_grupuri)
  
  observeEvent(input$grupuri_upload,{
  
  
    grupuri_reactive$file_input <-  input$grupuri_upload$datapath
  
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive=grupuri_reactive)
    
    
    output$show_grup_date <- renderUI({req(grupuri_reactive$all_names)
      if (grupuri_reactive$all_names==FALSE) {
        h5(paste("Lipseste coloana:",grupuri_reactive$missing_names,collapse = " ; "))}
      
      else {dateInput(inputId = session$ns("data_grupuri"),label = "Selecteaza data de constituire a grupurilor:",
                      value = Sys.Date(),width = "300px",autoclose = TRUE) }
      
      })
    
    observe({req(grupuri_reactive$all_names==TRUE)
      grupuri_reactive$fisier_prelucrat <- dplyr::mutate(grupuri_reactive$file_read_prel,
        data_constituire_grup = lubridate::make_date(year = paste0("20",
          stringr::str_sub(grupuri_reactive$file_read_prel$NrGrup,start = -2,end = -1)) %>% as.numeric(),
          month = stringr::str_sub(grupuri_reactive$file_read_prel$NrGrup,start = -4,end = -3) %>% as.numeric(),
          day = stringr::str_sub(grupuri_reactive$file_read_prel$NrGrup,start = -6,end = -5) %>% as.numeric())) })
      
    observeEvent(input$data_grupuri,{
      
      load('data/bi_grupuri.rda')
      
      grupuri_reactive$filtered_data <- dplyr::filter(.data = grupuri_reactive$fisier_prelucrat,
                  grupuri_reactive$fisier_prelucrat$data_constituire_grup <= input$data_grupuri) %>% 
                    dplyr::select(nume_obligatorii_grupuri,data_constituire_grup) %>%
                    dplyr::left_join(bi_grupuri,by = c("Nrcontract" = "Numar contract")) %>%
        dplyr::mutate(unic_tipologie = ifelse(tipologie_conventie %in% c("surse_proprii", "surse_administrare"),
                  "surse_proprii sau administrare",
                            ifelse(is.na(tipologie_conventie),"fara_contract","garantii_stat")))
      
      grupuri_reactive$grupuri_prelucrate <- grupuri_reactive$filtered_data %>% 
        dplyr::arrange(GrupId,unic_tipologie) %>%
        dplyr::group_by(GrupId,.drop = TRUE) %>% dplyr::summarise(unic_tipologie = paste(unique(unic_tipologie),collapse = " si ")) %>% 
        dplyr::ungroup()
      
      
      grupuri_reactive$grupuri_selectate <- grupuri_reactive$filtered_data %>% 
        dplyr::filter(GrupId %in% (grupuri_reactive$grupuri_prelucrate %>% 
              dplyr::slice(stringr::str_which(string = grupuri_reactive$grupuri_prelucrate$unic_tipologie,
                       pattern = "surse_proprii sau administrare")) %>% dplyr::pull(GrupId)))
      
      output$grupuri_selectate <- DT::renderDataTable({
        DT::datatable(data = grupuri_reactive$grupuri_selectate %>% dplyr::mutate_at(.vars = "tipologie_conventie",as.factor) %>% 
                        dplyr::select(nume_obligatorii_grupuri,data_constituire_grup,tipologie_conventie),
        rownames = FALSE,  caption = paste0("Lista grupurilor valabile la data de ",as.character(input$data_grupuri)),
                      filter = list(position = "top", clear = TRUE, plain = TRUE),
                      options = list(autoWidth = TRUE,scrollY=TRUE,scrollX=TRUE,
                      pageLength = 5,  searchHighlight=TRUE,fixedColumns = list(leftColumns = 4),
                      columnDefs = list(list(width = '100px', targets = 4),list(width='175px',targets = 3))))
      })
      
      removeUI("#database_grupuri_ui_1-down_grupuri_selectate")
      
      insertUI(selector = "#database_grupuri_ui_1-grupuri_selectate",where = "beforeBegin",
                        ui = fluidRow(downloadButton(outputId = session$ns("down_grupuri_selectate"),
                              label = "Download grupurile constituite"),br(),br()))
      
      output$down_grupuri_selectate <- downloadHandler(filename = function(){"grupuri_constituite.csv"},content = function(file){
        readr::write_csv(x = grupuri_reactive$grupuri_selectate,path = file)})
      
      
      grupuri_reactive$grupuri_neconstituite <- grupuri_reactive$filtered_data %>% 
        dplyr::filter(GrupId %in% (grupuri_reactive$grupuri_prelucrate %>% 
                    dplyr::slice(stringr::str_which(string = grupuri_reactive$grupuri_prelucrate$unic_tipologie,
           pattern = "surse_proprii sau administrare",negate = TRUE)) %>% dplyr::pull(GrupId))) 
      
      output$grupuri_neconstituite <- DT::renderDataTable({
        DT::datatable(data = grupuri_reactive$grupuri_neconstituite %>% dplyr::mutate_at(.vars = "tipologie_conventie",as.factor) %>% 
                        dplyr::select(nume_obligatorii_grupuri,data_constituire_grup,tipologie_conventie),
          rownames = FALSE,     caption = paste0("Lista grupurilor NEconstituite la data de ",as.character(input$data_grupuri)),
                      filter = list(position = "top", clear = TRUE, plain = TRUE),
                      options = list(autoWidth = TRUE,scrollY=TRUE,scrollX=TRUE,
                      pageLength = 5,  searchHighlight=TRUE,fixedColumns = list(leftColumns = 4),
                      columnDefs = list(list(width = '100px', targets = 4),list(width='175px',targets = 3))))   })
      
      removeUI("#database_grupuri_ui_1-down_grupuri_neconstituite")
      
      insertUI(selector = "#database_grupuri_ui_1-grupuri_neconstituite",where = "beforeBegin",
               ui = fluidRow(downloadButton(outputId = session$ns("down_grupuri_neconstituite"),
                                            label = "Download grupurile NEconstituite"),br(),br()))
      
      output$down_grupuri_neconstituite <- downloadHandler(filename = function(){"grupuri_NEconstituite.csv"},content = function(file){
        readr::write_csv(x = grupuri_reactive$grupuri_neconstituite,path = file)})
      
    }) 
     })
  
  
 
  
  
  
  
  
}
    
## To be copied in the UI
# mod_database_grupuri_ui("database_grupuri_ui_1")
    
## To be copied in the server
# callModule(mod_database_grupuri_server, "database_grupuri_ui_1")
 
