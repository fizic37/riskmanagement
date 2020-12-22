#' database_util_files UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_database_util_files_ui <- function(id){
  ns <- NS(id)
  
  shinydashboard::box(title = "Upload util files - BI, insolventa",status = "success",width = 12,collapsible = TRUE, collapsed = TRUE,
                      DT::dataTableOutput(outputId = ns("bi_sinteza")), br(),
                      fillRow(flex = c(1,NA),fileInput(inputId = ns("bi_input"),label = "Upload BI file",accept = c(".xlsx",".xls"),buttonLabel = "Excel only",
                                                       placeholder = "No file uploaded",width = "300px"),  br(), 
                              fluidRow(br(),downloadLink(outputId = ns("link_bi"),label = "Click aici pentru a downloada fisierul BI"))),
                      br(), br(),hr(),
                      fluidPage(textOutput(ns("mesaj_cereri_plata")),textOutput(ns("mesaj_instiintari"))), br(),
                      fileInput(inputId = ns("insolventa_input"),label = "Upload insolventa file",accept = ".csv",buttonLabel = "CSV only",
                                placeholder = "No file uploaded",width = "300px"), 
                      textOutput(ns("mesaj_insolvente")))
}
    
#' database_util_files Server Function
#'
#' @noRd 
mod_database_util_files_server <- function(input, output, session){
  ns <- session$ns
  
  
  bi_sinteza <- readRDS(file = "R/reactivedata/bi_sinteza.rds")
  
  vals <- reactiveValues(bi_sinteza = bi_sinteza)
  
  output$bi_sinteza <- DT::renderDataTable({dt_generate_function(vals$bi_sinteza,round_col = c(3,4,6))    })
  
  output$link_bi <- downloadHandler(filename = {"BI_calibrari.xlsx"},
                  content = function(file) {file.copy(from = "inst/extdata/BI_calibrari.xlsx",to = file)})
  
  observeEvent(input$bi_input,{
    
    removeUI("#database_util_files_ui_1-link_bi")
    
    #BI cereri plata 
    bi_citit_cereri_plata <- reactive({ shiny::validate(shiny::need(tools::file_ext(input$bi_input$datapath) == "xlsx", message = "XLSX only!"))
      shiny::validate(shiny::need("cereri_plata" %in% readxl::excel_sheets(input$bi_input$datapath),
                                  message = "Sunt programat sa citesc un sheet numit cereri_plata"))
      readxl::read_excel(input$bi_input$datapath, sheet = "cereri_plata", col_names = FALSE)  })
    
    bi_cereri_plata <- reactive ({ bi_citit_cereri_plata() %>% dplyr::slice(7:dplyr::n()) %>% setNames(.,c("ID Document","data_cerere_plata")) %>% 
        dplyr::mutate_at(.vars = 1,as.numeric) %>% dplyr::mutate(data_cerere_plata = stringr::str_sub(data_cerere_plata,1,10) %>% as.Date())  })
    
    snapshot_cereri_plata <- reactive({bi_citit_cereri_plata() %>% dplyr::slice(2) %>% dplyr::pull(2) %>% as.Date()   })
    
    # BI instiintari
    bi_citit_instiintari <- reactive({shiny::validate(shiny::need(tools::file_ext(input$bi_input$datapath) == "xlsx", message = "XLSX only!"))
      shiny::validate(shiny::need("instiintari" %in% readxl::excel_sheets(input$bi_input$datapath),
                                  message = "Sunt programat sa citesc un sheet numit instiintari" ))
      readxl::read_excel(input$bi_input$datapath, sheet = "instiintari", col_names = FALSE)  })
    
    bi_instiintari <- reactive ({ bi_citit_instiintari() %>% dplyr::slice(6:dplyr::n()) %>% setNames(.,c("ID Document","data_instiintare")) %>% 
        dplyr::mutate_at(.vars = 1,as.numeric) %>% dplyr::mutate(data_instiintare = stringr::str_sub(data_instiintare,1,10) %>% as.Date()) })
    
    snapshot_instiintari <- reactive({bi_citit_instiintari() %>% dplyr::slice(2) %>% dplyr::pull(2) %>% as.Date()   })
    
    if (snapshot_cereri_plata() >= dplyr::pull(bi_sinteza,1)) {
      
      vals$bi_sinteza$Snapshot_cereri_plata <- snapshot_cereri_plata()
      vals$bi_sinteza$Nr_observatii_cereri_plata <- nrow(bi_cereri_plata())
      
      output$mesaj_cereri_plata <- renderText({"Am salvat cu succes cererile de plata. 
        In tabelul de sinteza poti verifica valorile actualizate."})
      
      saveRDS(object = vals$bi_sinteza,file = "R/reactivedata/bi_sinteza.rds",version = 3,compress = "gzip")
      saveRDS(object = bi_cereri_plata (),file = "R/reactivedata/bi_cereri_plata.rds",version = 3,compress = "gzip")
      }
    
    if (dplyr::pull(bi_sinteza,2) >= snapshot_instiintari() & dplyr::pull(bi_sinteza,4) > nrow(bi_instiintari()) )  {
      
      output$mesaj_instiintari <- renderText({"Nu am salvat instiintarile de neplata. Desi ai uploadat
        un snapshot mai mare sau egal decat ce detin eu, am mai multe observatii in memorie. 
        Verifica BI-ul (filtrarea in cadrul sheet-ului instintari de neplata."})
    }  
    
    if (snapshot_instiintari() >=  dplyr::pull(bi_sinteza,2) & dplyr::pull(bi_sinteza,4) <= nrow(bi_instiintari())) {
      vals$bi_sinteza$Snapshot_instiintari_neplata <- snapshot_instiintari()
      vals$bi_sinteza$Nr_observatii_instiintari_neplata <- nrow(bi_instiintari())
      
      output$mesaj_instiintari <- renderText({"Am salvat cu succes instiintarile de neplata. 
        In tabelul de sinteza poti verifica valorile actualizate."})
      saveRDS(object = vals$bi_sinteza,file = "R/reactivedata/bi_sinteza.rds",version = 3,compress = "gzip")
      saveRDS(object = bi_instiintari (),file = "R/reactivedata/bi_instiintari.rds",version = 3,compress = "gzip")
      
    }
    
    if (snapshot_cereri_plata() < dplyr::pull(bi_sinteza,1)) {
      output$mesaj_cereri_plata <- renderText({"  Nu am salvat cererile de plata, am date mai recente in memorie."})
    }
    
    if (snapshot_instiintari() < dplyr::pull(bi_sinteza,2) )  {
      
      output$mesaj_instiintari <- renderText({"  Nu am uploadat instiintarile de neplata, am date mai recente in memorie."})
    }  
    
    
    
  })
  
  
  observeEvent(input$insolventa_input,{
    
    insolventa_prima_citire <- reactive({ readr::read_csv(input$insolventa_input$datapath) })
    
    insolventa_citit <- reactive({shiny::validate(shiny::need(c("Nume","Cod","DataInsolventa") %in% names(insolventa_prima_citire()),
        message = paste0("Lipsesc coloanele: ",paste0(setdiff(c("Nume","Cod","DataInsolventa"),names(insolventa_prima_citire()))))))
      
      readr::read_csv(input$insolventa_input$datapath, col_types = readr::cols(Cod = readr::col_character(), Nume = readr::col_character(),
              Tip = readr::col_skip(),"NrRegCom" = readr::col_skip(),CUI = readr::col_double(),
               'Introdus de' = readr::col_skip(), DataInsolventa =  readr::col_date(),
              Localitate = readr::col_skip(),   Judet = readr::col_skip(), Locatie = readr::col_skip(), Adresa = readr::col_skip())) %>% 
        dplyr::filter(!is.na(Cod),!is.na(DataInsolventa)) %>%   dplyr::group_by(Cod) %>% 
            dplyr::summarise(DataInsolventa=min(DataInsolventa,na.rm = TRUE))          })
    
    
    max_date_insolventa <- reactive({ shiny::validate(shiny::need(!is.na(max(insolventa_citit()$DataInsolventa)),
                        message = "STOP, nu pot prelucra DataInsolventa din fisierul uploadat"))
      max(insolventa_citit()$DataInsolventa,na.rm=TRUE)  })
    
    
    if (!is.na(max_date_insolventa()) && !is.na(nrow(insolventa_citit())) && 
        max_date_insolventa() >= dplyr::pull(vals$bi_sinteza,5))    {
      
      vals$bi_sinteza$Data_max_insolventa <- max_date_insolventa()
      
      vals$bi_sinteza$Nr_observatii_insolventa <- nrow(insolventa_citit())
      
      saveRDS(object = vals$bi_sinteza,file = "R/reactivedata/bi_sinteza.rds")
      saveRDS(object= insolventa_citit(),file = "R/reactivedata/insolventa.rds") 
      
      output$mesaj_insolvente <- renderText({"Am salvat cu succes fisierul de insolvente."})
    }
    
    else {output$mesaj_insolvente <- renderText({"NU am putut salva fisierul de insolvente. Downloadeaza fisierul CSV din Charisma 
      fara sa-l deschizi. "})}
      
  })
  
  
      
      
    
    
  
  
}
    
## To be copied in the UI
# mod_database_util_files_ui("database_util_files_ui_1")
    
## To be copied in the server
# callModule(mod_database_util_files_server, "database_util_files_ui_1")
 
