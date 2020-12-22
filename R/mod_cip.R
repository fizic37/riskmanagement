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
  tagList(
    shinydashboard::box(title = "CIP Database",status = "success",width = 12,collapsible = T,collapsed = FALSE,
                        DT::dataTableOutput(ns("cip_database")),
                        br(),
                        fluidPage(fillRow(flex = c(1,NA),downloadButton(outputId = ns("down_cip_database"),label = "Download CIP database"),
                              actionButton(inputId = ns("delete_CIP_database"),label = "Delete observations",my_icon = "minus-square")),
                              br(),
                          uiOutput(ns("cip_input_output")),
                          DT::dataTableOutput(ns("cip_upload_sumar")),br(),
                          uiOutput(ns("save_cip_output")),
                        ))
  )
                        
}
    
#' cip Server Function
#'
#' @noRd 
mod_cip_server <- function(input, output, session) {
  ns <- session$ns
 load("data/baza_date_cip.rda")
 
 output$cip_database <- DT::renderDataTable({dt_generate_function(round_col = 2:5, caption = "Sinteza baza date CIP:",
   df = baza_date_cip %>% dplyr::group_by(data_raport) %>% dplyr::summarise(Nr_beneficiari_raportati = dplyr::n_distinct(CUI),
      Nr_beneficiari_interdictie=sum(Este_in_interdictie),
       Nr_mediu_incidente_majore=mean(total_incidente_majore),Median_incidente_majore=median(total_incidente_majore)))  })
 
 output$cip_input_output <- renderUI({
   fileInput(inputId = session$ns("cip_input"),accept = '.csv',buttonLabel = 'CSV only',label = "Upload CIP file",width = "300px") })
 
 observeEvent(input$cip_input,{
   
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
    
     load("data/baza_date_cip.rda")
     unique_cip_database_dates <- unique(baza_date_cip$data_raport)
     
     check_type_cip <- reactive({janitor::compare_df_cols_same(baza_date_cip,CIP(),bind_method = "rbind",verbose = FALSE) })
     
     if (check_type_cip() && !data_upload_cip() %in% unique_cip_database_dates) {
       baza_date_cip <- isolate(rbind(CIP(),baza_date_cip))
       
       usethis::use_data(baza_date_cip,version = 3,compress = "gzip",internal = FALSE,overwrite = TRUE)
       
       load("data/baza_date_cip.rda")
       
       output$cip_database <- DT::renderDataTable({dt_generate_function(round_col = 2:5, caption = "Sinteza baza date CIP actualizata:",
            df = baza_date_cip %>% dplyr::group_by(data_raport) %>% dplyr::summarise(Nr_beneficiari_raportati = dplyr::n_distinct(CUI),
            Nr_beneficiari_interdictie=sum(Este_in_interdictie), Nr_mediu_incidente_majore=mean(total_incidente_majore),
            Median_incidente_majore=median(total_incidente_majore)))  })
     }
     
     else if (data_upload_cip() %in% unique_cip_database_dates){
       shinyWidgets::sendSweetAlert(session = session,title = "STOP",text = "Am deja data furnizata de tine",type = "warning",
                                    btn_colors = "#577a54",btn_labels = "OK, am inteles.") }
     })
 })
       
  observeEvent(input$delete_CIP_database,{
    load("data/baza_date_cip.rda")
    shinyWidgets::inputSweetAlert(session = session,inputId = session$ns("select_date_cip"),
                                  title = 'Selecteaza data raportului pe care vrei sa-l stergi',input = "select",
                                  inputOptions = unique(baza_date_cip$data_raport),
                                  type = "warning",btn_colors = "#577a54",btn_labels = "OK, am selectat data") }) 
  
  observeEvent(input$select_date_cip,{
    baza_date_cip <- isolate(dplyr::filter(baza_date_cip,data_raport != as.Date.numeric(x = as.numeric(input$select_date_cip),
                                                                                        origin = "1970-01-01")))
    
    usethis::use_data(baza_date_cip,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
    
    load("data/baza_date_cip.rda")
    
    output$cip_database <- DT::renderDataTable({dt_generate_function(round_col = 2:5, caption = "Sinteza baza date CIP actualizata:",
                                                                     df = baza_date_cip %>% dplyr::group_by(data_raport) %>% 
                                                                       dplyr::summarise(Nr_beneficiari_raportati = dplyr::n_distinct(CUI),
                                                                                        Nr_beneficiari_interdictie=sum(Este_in_interdictie),Nr_mediu_incidente_majore=mean(total_incidente_majore),
                                                                                        Median_incidente_majore=median(total_incidente_majore)))  })
  })
  
  output$down_cip_database <- downloadHandler(filename = function(){"cip_database.csv"},content = function(file){
    load("data/baza_date_cip.rda")
    readr::write_csv(x = baza_date_cip,path = file)
})
 
}
 
    
## To be copied in the UI
# mod_cip_ui("cip_ui_1")
    
## To be copied in the server
# callModule(mod_cip_server, "cip_ui_1")
 
