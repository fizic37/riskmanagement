#' cip UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cip_ui <- function(id){
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  
  shinydashboard::box(title = "CIP Database",status = "success",width = 12,collapsible = T,collapsed = FALSE,
                        
    fluidRow( 
              
    DT::dataTableOutput(ns("cip_database")),
      
            tags$script(src = "cip_delete_button.js"),
            tags$script(paste0("cip_delete_module_js('", ns(''), "')")),
                        
      hr(),
                      
    fluidRow(
      column(width = 6,fileInput(inputId = ns("cip_input"),
              accept = '.csv',buttonLabel = 'CSV only',label = "Upload CIP file",width = "300px")),
      column(width = 6, br(), uiOutput(ns("save_cip_output")))),
                        
    DT::dataTableOutput(ns("cip_upload_sumar"))
    
    )  )
  
                        
}
    
#' cip Server Function
#'
#' @noRd 
mod_cip_server <- function(input, output, session) {
  ns <- session$ns
 
  load("data/view_cip_database.rda")
  
  vals_cip <- reactiveValues(view_cip_database = view_cip_database)
  
  
  # Key observer. Everytime it updates I update actions and render teh main table.
  # Note I do not save it, as it will be saved from the beginning. I only save it inside vals_cip$baza_date_cip observer
  observeEvent(vals_cip$view_cip_database,{
    
    vals_cip$unique_dates <-  vals_cip$view_cip_database$data_raport
    
    vals_cip$actions <- purrr::map_chr(vals_cip$unique_dates, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-sm download_btn" data-toggle="tooltip" data-placement="top" title="Download" id = ', id_, ' style="margin: 0"><i class="fa fa-download"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )  })
    
    output$cip_database <- DT::renderDataTable({DT::datatable(caption = "Sinteza baza date CIP:",escape = FALSE,rownames = F,
               data = cbind(tibble::tibble(" " = vals_cip$actions), vals_cip$view_cip_database),
                       options = list(dom = "tp",pageLength=5)) %>% DT::formatRound(3:6,digits = 0)  })
    
    })
  

  observeEvent(input$id_data_raport_to_download,{
     showModal(modalDialog(title = h3("ATENTIE!",style = "color: #ffa500;"), size = "l",
                        h3(paste0("Esti sigur ca vrei sa downloadezi baza de date CIP raportata la ",
                                        input$id_data_raport_to_download, " ?"), style = "color: #77547a"),  footer = 
      tagList(downloadButton(outputId = session$ns("down_cip_database"),label = "Download"),
              modalButton(label = "Cancel",icon = icon("window-close")))))
    
    output$down_cip_database <- downloadHandler(filename = function(){paste0("cip_database_input$id_data_raport_to_download",".csv")},
        content = function(file){
          load("data/baza_date_cip.rda")
            readr::write_csv(x = baza_date_cip %>% dplyr::filter(data_raport == as.Date.character(input$id_data_raport_to_download)),
                path = file) })
    
    })
  
  
    
  # Key observer everytime baza date cip is updated: recalculates view_cip and saves baza_date_cip and view_cip
  # Attention: I save view_cip only when baza date cip is updated, otherwise it would be saved even for the first read
  observeEvent(vals_cip$baza_date_cip,{
    
    vals_cip$view_cip_database <- vals_cip$baza_date_cip %>% dplyr::group_by(data_raport) %>% dplyr::summarise(
      Nr_beneficiari_raportati = dplyr::n_distinct(CUI),
      Nr_beneficiari_interdictie =
        sum(Este_in_interdictie),
      Nr_mediu_incidente_majore =
        mean(total_incidente_majore),
      Median_incidente_majore = median(total_incidente_majore)) %>% dplyr::arrange(dplyr::desc(data_raport))
    
    baza_date_cip <- isolate(vals_cip$baza_date_cip)
    view_cip_database <- isolate(vals_cip$view_cip_database)
    usethis::use_data(baza_date_cip,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    usethis::use_data(view_cip_database,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
  })

 
  # Observer for CIP upload. I have used reactive() so save upload observer is nested inside upload observer in order to 
 # have access to values stored with reactive(). Note that it does not renders with multiple uploads inside the same session if
  # it had been already saved once.
  observeEvent(input$cip_input,{
   
   # Main processing line of cip upload file
   CIP <- reactive({shiny::validate(shiny::need(tools::file_ext(input$cip_input$datapath)=="csv",message = paste0("CSV only. You uploaded a ", 
                       tools::file_ext(input$cip_input$datapath), " file.")))
     readr::read_csv(input$cip_input$datapath, col_types = readr::cols(
       `Data situatie risc global` = readr::col_date(format = "%m/%d/%Y 12:00:00 AM"),
       `Cod debitor` = readr::col_double(),  `Nume debitor` = readr::col_character(),
      `Total CEC-uri` = readr::col_double(),  `Total cambii` = readr::col_double(),
       `Total bilete la ordin` = readr::col_double(),`CEC-uri majore` = readr::col_double(),
        `Cambii majore` = readr::col_double(), `Bilete la ordin majore` = readr::col_double(),
              `Per de interdictie bancara` = readr::col_character())) %>% dplyr::mutate(
                             total_incidente = `Total CEC-uri` + `Total cambii` + `Total bilete la ordin`,
                             total_incidente_majore = `Bilete la ordin majore` + `CEC-uri majore` + `Cambii majore`,
                lista_date = qdapRegex::ex_date(`Per de interdictie bancara`)) %>% 
       dplyr::mutate(lungime_lista_date = purrr::map_dbl(lista_date, ~ length(.))) %>% 
       dplyr::mutate(data_expirare = dplyr::case_when(is.na(`Per de interdictie bancara`) ~ as.Date("1999-01-01", 
        format = "%Y-%m-%d"), !is.na(`Per de interdictie bancara`) & lungime_lista_date == 1 ~ as.Date.numeric(
          as.numeric(`Per de interdictie bancara`), origin = "1899-12-30") %>% as.Date(),
        !is.na(`Per de interdictie bancara`) & lungime_lista_date != 1 ~ qdapRegex::ex_date(`Per de interdictie bancara`) %>%
          purrr::map_chr(~ .x[[length(.x)]]) %>% as.Date(format = "%d-%m-%Y")),
          data_intrare = dplyr::case_when(is.na(`Per de interdictie bancara`) | lungime_lista_date == 1 ~ as.Date("1999-01-01",
            format = "%Y-%m-%d"),!is.na(`Per de interdictie bancara`) & lungime_lista_date > 1 ~ qdapRegex::ex_date(`Per de interdictie bancara`) %>% 
              purrr::map_chr(~ .x[[max(length(.x) - 1, 1)]]) %>% as.Date(format = "%d-%m-%Y"))) %>% 
                dplyr::mutate(Este_in_interdictie = ifelse(data_expirare >= `Data situatie risc global` &  data_intrare <= `Data situatie risc global`,1,0),
                              A_fost_interdictie = ifelse(is.na(`Per de interdictie bancara`), 0, 1)) %>% 
                  dplyr::select(CUI=`Cod debitor`,total_incidente,  total_incidente_majore,Este_in_interdictie,A_fost_interdictie,
                     bilete_majore = `Bilete la ordin majore`,data_raport=`Data situatie risc global`)   })
   
   cip_upload_sumar <- reactive({ CIP() %>% dplyr::group_by(data_raport) %>% dplyr::summarise(Nr_beneficiari_raportati = dplyr::n_distinct(CUI),
              beneficiari_interdictie=sum(Este_in_interdictie), Nr_mediu_incidente_majore=mean(total_incidente_majore),
              Median_incidente_majore=median(total_incidente_majore)) %>% dplyr::ungroup() })
   
   
   data_upload_cip <- reactive({ cip_upload_sumar()$data_raport  })
   
   output$cip_upload_sumar <- DT::renderDataTable({req(cip_upload_sumar())
     dt_generate_function(round_col = 2:5,caption = "Sinteza fisier uploadat:", df = cip_upload_sumar()) })
   

  output$save_cip_output <- renderUI({req(cip_upload_sumar())
   actionButton(inputId = ns("save_cip_upload"),label = "Save CIP upload",icon = icon("save"),width = "300px")  })
  
 
  

  observeEvent(input$save_cip_upload,{
      removeUI("#cip_ui_1-save_cip_upload")
      removeUI("#cip_ui_1-cip_upload_sumar")
      removeUI("#cip_ui_1-cip_input_output")  
      
      
     unique_cip_database_dates <- unique(view_cip_database$data_raport)
     
     # I load slice data in order to check compatibility of upload data with main cip database
     # through janitor-bind_rows with slice data. Upload main database only if this compatibility is true
     load("data/slice_cip_database.rda")
     
     check_type_cip <- reactive({janitor::compare_df_cols_same(slice_cip_database,CIP(),
                                                               bind_method = "rbind",verbose = FALSE) })
     
     if (check_type_cip() && !data_upload_cip() %in% unique_cip_database_dates) {
       
       load("data/baza_date_cip.rda")
       
       vals_cip$baza_date_cip <- rbind(CIP(),baza_date_cip)
      }
     
     else if (data_upload_cip() %in% unique_cip_database_dates){
       shinyWidgets::sendSweetAlert(session = session,title = "STOP",text = "Am deja data furnizata de tine",type = "warning",
                                    btn_colors = "#577a54",btn_labels = "OK, am inteles.") }
     })
  
 })
 
  observeEvent(input$id_data_raport_to_delete,{
  shinyWidgets::ask_confirmation(inputId = session$ns("confirm_delete"),title = "ATENTIE",
      text = paste0("Esti sigur ca vrei sa stergi toate inregistrarile CIP aferente raportarii din data ",
             input$id_data_raport_to_delete," ?"),type = "warning",btn_colors = c("#dd4b39","#008000"))
})       
  
  
  
  observeEvent(input$confirm_delete,{
    
    load("data/baza_date_cip.rda")
    vals_cip$baza_date_cip <- dplyr::filter(baza_date_cip,data_raport != 
                                              as.Date.character(input$id_data_raport_to_delete))
   })
  
  
}
 
    
## To be copied in the UI
# mod_cip_ui("cip_ui_1")
    
## To be copied in the server
# callModule(mod_cip_server, "cip_ui_1")
 
