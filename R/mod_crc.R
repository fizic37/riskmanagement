#' crc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_crc_ui <- function(id){
  ns <- NS(id)
  tagList(
        shinydashboard::box(title = "CRC Database",status = "success",width = 12,collapsible = T,collapsed = F,
            footer = "*Scorul maxim al serviciului datoriei este de 50%",
               DT::dataTableOutput(ns("sumar_baza_date_crc")) %>% shinycssloaders::withSpinner(color = "#77547a"),
                    br(),  uiOutput(ns("action_baza_date_crc"))),
                            
        
        shinydashboard::box(title="Upload new CRC file",status = "success",width = 12, collapsible = T, collapsed = F,#height = "500px",
                           shinyjs::useShinyjs(),
                             fileInput(inputId = ns("crc_input"),label = "Upload CRC file",accept = ".csv",
                                      buttonLabel = "csv only",placeholder = "no file uploaded",width = "300px"),
                            DT::dataTableOutput(ns("sumar_crc_upload")),
                            DT::dataTableOutput(ns("dobanzi_crc")),
                            br(),
                            fillRow(flex = c(1,NA),uiOutput(ns("down_crc_output")),uiOutput(ns("save_crc_output"))),hr(),
                            textOutput(ns("error_crc_type")))
                            
        )
              
        
}
    
#' crc Server Function
#'
#' @noRd 
mod_crc_server <- function(input, output, session){
  ns <- session$ns
  
  load('data/view_baza_date_crc.rda')
  
  unique_dates_crc_database <- unique(view_baza_date_crc$`Data situatie risc global`)
  
  vals <- reactiveValues(view_baza_date_crc = view_baza_date_crc,
                         unique_dates_crc_database = unique_dates_crc_database)
  
  
  threshold_date_input_crc <- as.Date("2018-12-31")
  
  
  output$sumar_baza_date_crc <- DT::renderDataTable({dt_generate_function(df=vals$view_baza_date_crc,
              round_col = 2:4, perc_col = 5:7, caption = "Sinteza baza date CRC:") })
  
  output$action_baza_date_crc <- renderUI({#req(sumar_baza_date_crc)
    fluidRow(column(width = 4,downloadButton(outputId = ns("down_baza_date_crc"),label = "Download CRC database")),
            column(width = 4,actionButton(inputId = ns("delete_baza_date_crc"),width = "220px",
              label = "Sterge date CRC",icon = icon("minus-square"))), 
            column(width = 4,actionButton(inputId = ns('archive_crc'),width = "220px", class = "pull-right",
                                          label = "Show all archived data",icon = icon("file-archive"))))  })
  
  output$down_baza_date_crc <- downloadHandler(filename = function() {'baza_date_crc.csv'},
                content = function(file) {readr::write_csv(x = vals$baza_date_crc,path = file)} )
  
  
  
  observeEvent(input$delete_baza_date_crc,{
  
    shinyWidgets::inputSweetAlert(session = session,inputId = session$ns("alert_delete_crc"),
              inputOptions = vals$unique_dates_crc_database,btn_labels = "OK",btn_colors = "#577a54",
                  title = "Selecteaza data raportului",input = "select",type = "warning")
  })
    
  observeEvent(input$alert_delete_crc,{
      
      withProgress(expr = {
      
      load("data/baza_date_crc.rda")
      
      vals$baza_date_crc <- dplyr::filter(baza_date_crc,
        `Data situatie risc global` != as.Date.numeric(as.numeric(input$alert_delete_crc),origin = "1970-01-01"))
      
      baza_date_crc <- isolate(vals$baza_date_crc)
      
      usethis::use_data(baza_date_crc,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3) 
      
     },message = "Processing CRC database for deletion")      })
      
  
  observeEvent(vals$baza_date_crc,{req(vals$baza_date_crc)
    
      vals$view_baza_date_crc  <- sumar_baza_date_crc_func(df = vals$baza_date_crc)
      
      vals$unique_dates_crc_database <- unique(vals$view_baza_date_crc$`Data situatie risc global`)
      
      view_baza_date_crc <- isolate(vals$view_baza_date_crc)
      
      usethis::use_data(view_baza_date_crc,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
      
      })
    
  observeEvent(input$crc_input,{
    
    load("data/rata_dobanzii.rda")
    load('data/baza_date_crc_sliced.rda')
    
    ratele_dobanzii_valabile <-  rata_dobanzii %>% dplyr::select(2:4) %>% 
      tidyr::pivot_wider(names_from = MONEDA,values_from = `Rata dobanzii`)
    
    crc_input_citit <- reactive({  shiny::validate(shiny::need(tools::file_ext(input$crc_input$datapath)=="csv",
                              message = paste0("CSV only. You uploaded a ",tools::file_ext(input$crc_input$datapath)," file.")))
      readr::read_csv(input$crc_input$datapath) })
    
    nume_obligatorii <- c('Data situatie risc global', 'Unitate bancara', 'Cod debitor','Acronim persoana declaranta',
    'Cod CRC', 'Comportament credit','Tip Credit', 'Termen acordare', 'Serviciul datoriei',
                          'Cod Valuta', 'Suma acordata', 'Suma datorata total','Suma datorata utilizata',
                           'Suma restanta', 'Data acordarii','Data scadentei')
    
    crc_csv <- reactive ({ shiny::validate(shiny::need(expr = nume_obligatorii %in% names(crc_input_citit()) %>% all(),
          message = paste0("Lipsesc coloanele: ",paste0(setdiff(nume_obligatorii, names(crc_input_citit())),collapse = ";"))))
      
      readr::read_csv(input$crc_input$datapath,    col_types = readr::cols(     'Data acordarii' = readr::col_date(format = "%d/%m/%Y"),
        'Data scadentei' = readr::col_date(format = "%d/%m/%Y"),  
          `Data situatie risc global` = readr::col_date(format = "%m/%d/%Y 12:00:00 AM"),
          'Comportament credit' = readr::col_character())  ) %>% 
            dplyr::select(nume_obligatorii) %>% dplyr::filter(`Unitate bancara` == "*") %>%
              dplyr::mutate( valuta_termen = paste0(`Cod Valuta`, stringr::str_sub(`Termen acordare`, start = 1, end = 1  )),
                Principal_de_Rambursat = ifelse(`Comportament credit` == "L",  `Suma restanta`,
                ifelse(`Data scadentei` <= lubridate::`%m+%`(`Data situatie risc global`,months(12)),   `Suma datorata total`,
                    pmax(`Suma restanta`, (`Suma datorata total` * 365) / as.numeric(difftime(
                     time2 = `Data situatie risc global`,  time1 = `Data scadentei`, units = "days"))))))    })
    
    crc_csv_rata_dobanzii <-  reactive ({ crc_csv() %>% dplyr::mutate(rata_dobanzii = 
                          rata_dobanzii$`Rata dobanzii`[match(x = crc_csv()$valuta_termen,
                                        table = rata_dobanzii$valuta_termen)]) %>%
        dplyr::mutate(Dobanda_de_Rambursat = rata_dobanzii * (2 * `Suma datorata total` - Principal_de_Rambursat) / 2) %>%
        dplyr::mutate(Rate_datorate = Principal_de_Rambursat + Dobanda_de_Rambursat)  })  
    
    crc_cui <-  reactive ({  crc_csv_rata_dobanzii() %>% dplyr::group_by(`Cod debitor`,`Serviciul datoriei`) %>%  
        dplyr::summarise(suma_totala_utilizata = sum(`Suma datorata utilizata`)) %>% 
        tidyr::spread(key = "Serviciul datoriei", value = suma_totala_utilizata, fill = 0) %>% as.data.frame() %>% 
        dplyr::mutate(total_sume_utilizate = rowSums(dplyr::select(., -1))) %>%
        dplyr::mutate(scor_serv_datorie=(0.5*.[[2]]+0.25*.[[3]]+0.15*.[[4]]+0.1*.[[5]])/(total_sume_utilizate)) %>% 
        dplyr::mutate(scor_serv_datorie = ifelse(is.na(scor_serv_datorie),0.5,scor_serv_datorie)) %>%
        dplyr::mutate(are_restante_peste_30_zile = ifelse((.[[2]]+.[[3]])/(total_sume_utilizate)>= 0.95,0,1)) %>% 
        dplyr::mutate(are_restante_peste_30_zile = ifelse(is.na(are_restante_peste_30_zile),0,are_restante_peste_30_zile))  })
    
    crc_final <-  reactive ({ dplyr::left_join(y = crc_cui() %>% dplyr::select(.,`Cod debitor`,
            scor_serv_datorie,are_restante_peste_30_zile),  x=crc_csv_rata_dobanzii(),by="Cod debitor")   })
    
    sumar_crc_upload <- reactive({ crc_final() %>% dplyr::group_by(`Cod debitor`, `Data situatie risc global`) %>% 
        dplyr::summarise(Nr_beneficiari = dplyr::n_distinct(`Cod debitor`),
      Total_rate_datorate =   sum(Rate_datorate),
      Rate_medii_datorate = sum(Rate_datorate),
      are_restante_peste_30_zile = mean(are_restante_peste_30_zile),
      scor_mediu_serv_datorie = mean(scor_serv_datorie),
      sume_totale_utilizate = sum(`Suma datorata utilizata`)) %>%
        dplyr::group_by(`Data situatie risc global`) %>% dplyr::summarise(`Nr beneficiari` = sum(Nr_beneficiari),
          `Total rate datorate` = sum(Total_rate_datorate),    `Rate datorate medii per beneficiar` =      mean(Rate_medii_datorate),
          `Nr mediu beneficiari cu restante peste 30 de zile` = mean(are_restante_peste_30_zile),
          `Scor mediu serviciul datoriei` = mean(scor_mediu_serv_datorie),
          `Scor mediu serviciul datoriei ponderat cu sumele utilizate` = 
            weighted.mean(scor_mediu_serv_datorie, w = sume_totale_utilizate))     })
    
    output$sumar_crc_upload <- DT::renderDataTable({req(input$crc_input)
      dt_generate_function(df = sumar_crc_upload(),round_col = 2:4,perc_col = 5:7,caption = "Sinteza fisier uploadat:")   })
    
    output$dobanzi_crc <- DT::renderDataTable({req(sumar_crc_upload)
      dt_generate_function(df=ratele_dobanzii_valabile,perc_col = 2:5,digits_perc=2, round_col = NULL,
                  caption = "Voi folosi ratele dobanzii de mai jos pentru a calcula ratele datorate aferente fisierului uploadat:")     })
    
    vals$check_type_crc <- janitor::compare_df_cols_same(baza_date_crc_sliced, crc_final() %>% 
              dplyr::select(-`Cod CRC`, - `Acronim persoana declaranta`), bind_method = "rbind",verbose = FALSE)
    
   
    output$save_crc_output <- renderUI({req(vals$check_type_crc)
      actionButton(inputId = session$ns("save_crc"),label = "Save CRC uploadat to database",icon = icon("save"))  })
    
    output$error_crc_type <- renderPrint({req(!vals$check_type_crc)
      janitor::compare_df_cols_same("existing database"=baza_date_crc_sliced,"uploaded file" = crc_final() %>% 
            dplyr::select(-`Cod CRC`, - `Acronim persoana declaranta`), bind_method = "rbind",verbose = TRUE) })
    
    output$down_crc_output <- renderUI({req(vals$check_type_crc)
      downloadButton(outputId = ns("down_crc_input"),label = "Download CRC uploadat")     })
    
    output$down_crc_input <- downloadHandler(filename = function(){"CRC.csv"},content = function(file){
      readr::write_csv(x = crc_final() %>% dplyr::select(-`Unitate bancara`, - `Tip Credit`, -valuta_termen) %>%
                         dplyr::relocate(Principal_de_Rambursat,.after = rata_dobanzii), path=file) }  )
    
    data_crc_upload <- reactive({ crc_final()$`Data situatie risc global`[1] })
    
  observeEvent(input$save_crc,{
    #removeUI("#crc_ui_1-save_crc")
    
    removeUI("#crc_ui_1-dobanzi_crc")
    
   if (data_crc_upload() <= threshold_date_input_crc) {
      shinyWidgets::sendSweetAlert(session = session,title = "STOP",text = "Nu pot salva fisiere anterioare datei de 01 Ianuarie 2019",
                                   type = "error",showCloseButton = TRUE)  }
    
    else if (data_crc_upload() %in% vals$unique_dates_crc_database) {
      shinyWidgets::inputSweetAlert(session = session,inputId = "overwrite_CRC_input",input = "radio",
                  inputOptions = c("Overwrite?","Renunta!"),btn_colors = "#577a54",btn_labels = "OK",
                  title = "ATENTIE",type = "warning",showCloseButton = FALSE,
                text = "Am deja data selectata de tine in baza mea de date")  }
    
    else { 
      
        withProgress(expr = {
        load("data/baza_date_crc.rda")
        
        vals$baza_date_crc <- dplyr::bind_rows(baza_date_crc,crc_final())
        
        baza_date_crc <- isolate( vals$baza_date_crc)
       
        usethis::use_data(baza_date_crc,internal = FALSE,overwrite = TRUE,version = 3,compress = "gzip") 
        
        shinyjs::reset("save_crc")
        }, message = "Processing CRC database for addition")
        
        shinyWidgets::sendSweetAlert(session = session,title = "SUCCES", type ="success",showCloseButton = TRUE,
                                   text = "Baza de date a fost uploadata cu succes") 
        
      
    }
 
    observeEvent(input$overwrite_CRC_input,{
      if (input$overwrite_CRC_input=="Renunta!") {
        shinyWidgets::closeSweetAlert(session = session) }
      
      else if(vals$check_type_crc)  {
        
        withProgress(expr = {
          load("data/baza_date_crc.rda")
          
          vals$baza_date_crc <- dplyr::bind_rows(dplyr::filter(baza_date_crc,`Data situatie risc global` != data_crc_upload()),
                                                 crc_final())
         
          baza_date_crc <- isolate(vals$baza_date_crc)
          
         usethis::use_data(baza_date_crc,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3) 
         
         shinyjs::reset("save_crc")  }, message = "Processing CRC database for replacement")
        
        shinyWidgets::sendSweetAlert(session = session,title = "SUCCES", type ="success",showCloseButton = TRUE,
                                     text = "Baza de date a fost uploadata cu succes")
        
      }
    })
    
    
  })
  
  })
  
  
}
    
## To be copied in the UI
# mod_crc_ui("crc_ui_1")
    
## To be copied in the server
# callModule(mod_crc_server, "crc_ui_1")
 
