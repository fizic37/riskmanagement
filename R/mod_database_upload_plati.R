#' database_upload_plati UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_database_upload_plati_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::box(title = "Baza de date a provizioanelor pentru plati",collapsed = TRUE,status = "success",width=12,collapsible = T,
        DT::dataTableOutput(ns("database_plati")),br(),
        fillRow(flex = c(1,NA),downloadButton(outputId = ns("down_database_plati"),
                label = "Click aici pentru a downloada baza de plati"), 
          actionButton(inputId = ns("delete_plati"),label = "Delete observations",icon = icon("minus-square"),width="300px"))
        ),
    
    
    shinydashboard::box(title="Upload plati file",status = "success",width = 12,collapsible = TRUE,collapsed = FALSE,
                        shinyjs::useShinyjs(),
        tagList(fluidRow(column(width = 6,  fileInput(inputId = ns("plati_input"), width = "500px",    
                        label = "Upload provizioane plati file",
                        buttonLabel = "Excel Only",       placeholder = "No file uploaded",accept = c(".xls",".xlsx") )),
                        column(width = 6,uiOutput(ns("show_plati_date")))),
        
        
        DT::dataTableOutput(ns("sumar_plati_input")),br(), 
        uiOutput(ns("output_save_plati")), br(),
        
        fileInput(ns("balanta_parteneri_input"), width = "500px",
                  label = "Upload baza de date parteneri - Doar dupa ce s-a uploadat fisierul de plati",
                  accept = c(".xls",".xlsx"),buttonLabel = "Excel only",placeholder = "no file uploaded")),
        DT::dataTableOutput(ns("tabel_diferente"))),
    
    shinydashboard::box(title = "Regularizare provizion plati",status = "success",width = 12,collapsible = T,collapsed = FALSE,
        tagList(column(width = 6, dateInput(inputId = ns("from_plati"),label = "Selecteaza data initiala:",
                                            value = Sys.Date(),width = "300px")),
                column(width = 6, dateInput(inputId = ns("to_plati"),label = "Selecteaza data curenta:",
                                            value = Sys.Date(),width = "300px")),
                column(width = 6,downloadButton(outputId = ns("generate_report"),label = "Generate adresa economic - Word"))
        ))
        
    )
  
}
    
#' database_upload_plati Server Function
#'
#' @noRd 
mod_database_upload_plati_server <- function(input, output, session){
  ns <- session$ns
  
  shinyjs::disable("balanta_parteneri_input")
  
  threshold_date_plati_input <- as.Date("2019-12-31")
  
  load('data/view_sumar_plati.rda')
  load('data/slice_provizioane_plati.rda')
  
  
  plati_reactive <- reactiveValues(sumar_plati = view_sumar_plati,
                                   
                                   nume_obligatorii = c("CodBeneficar",slice_provizioane_plati %>%
                                     dplyr::select( -Plata_neta,-Expunere_CTG_plati,-data_raport) %>% names()),
                                   
                                   column_names_date = slice_provizioane_plati %>% 
                                     dplyr::select(dplyr::contains("data")) %>% dplyr::select(-data_raport) %>% names())
  
  
  output$database_plati <- DT::renderDataTable({dt_generate_function(df = plati_reactive$sumar_plati, round_col = 2:8,perc_col = 9,
                                    caption = "Sumar baza date Provizioane Plati:") })
  
  updateDateInput(session = session,inputId = "from_plati", value = tail(plati_reactive$sumar_plati$data_raport,2)[1])
  updateDateInput(session = session,inputId = "to_plati", value = tail(plati_reactive$sumar_plati$data_raport,1))
  
  observeEvent(c(input$from_plati,input$to_plati),{
    text_data_directory <- "data"
    output$generate_report <- downloadHandler(filename = function() {"regularizare_proviz_plati.docx"},content = function(file){
      rmarkdown::render(input = "test_word.Rmd",
                        output_format = "word_document",
                        output_file = "data/rezultat_word.docx",
                        output_dir = getwd(),
                        run_pandoc = T,
                        params = list(luna_curenta = lubridate::month(input$to_plati,label = TRUE,abbr = FALSE),
                                      an_curent = lubridate::year(input$to_plati),
                                      provizion_curent = plati_reactive$sumar_plati$Provizioane_Constituite[
                                                    plati_reactive$sumar_plati$data_raport == input$to_plati],
                                      provizion_anterior = plati_reactive$sumar_plati$Provizioane_Constituite[
                                                    plati_reactive$sumar_plati$data_raport == input$from_plati],
                                      nr_garantii = plati_reactive$sumar_plati$Nr_contracte[
                                                    plati_reactive$sumar_plati$data_raport == input$to_plati],
                                      luna_anterioara = lubridate::month(input$from_plati,label = TRUE,abbr = FALSE),
                                      an_anterior = lubridate::year(input$to_plati),
                                      plati_brute_curente = plati_reactive$sumar_plati$PlatiBrute[
                                                    plati_reactive$sumar_plati$data_raport == input$to_plati],
                                      recuperari_totale_curente = plati_reactive$sumar_plati$RecuperariTotale[
                                                    plati_reactive$sumar_plati$data_raport == input$to_plati],
                                    admise_garantie_curente = plati_reactive$sumar_plati$Garantii_accesorii_admise[
                                                    plati_reactive$sumar_plati$data_raport ==  input$to_plati],
                                    data_curenta = paste(lubridate::day(input$to_plati),lubridate::month(input$to_plati),
                                                         lubridate::year(input$to_plati),sep = "."),
                                    data_anterioara =  paste(lubridate::day(input$from_plati),lubridate::month(input$from_plati),
                                                             lubridate::year(input$from_plati),sep = ".")),
                        encoding = "UTF-8") %>% readBin(con = "rezultat_word.docx",
                                                        what = "raw",
                                                        n = file.info("rezultat_word.docx")[, "size"]) %>%  writeBin(con = file)
    }) 
  })
  
  
  
  observeEvent(input$plati_input,{
    shiny::validate(shiny::need(any(tools::file_ext(input$plati_input$datapath) %in% c("xls", "xlsx")),
                                message = paste0("Excel only, you uploaded a ",  tools::file_ext(input$plati_input$datapath)," file")))
    
    shinyjs::enable("balanta_parteneri_input")
    
    plati_reactive$file_input = input$plati_input$datapath
  
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive = plati_reactive)
    
    # Below observer activates after excel module is called
    
        observe({req(plati_reactive$all_names == TRUE)
      
        plati_reactive$file_read_prel <- plati_reactive$file_read_prel %>%  dplyr::filter(!is.na(DocumentId)) %>% 
                      dplyr::mutate_at(.vars = 'CUI',as.numeric) 
        
        plati_reactive$data_plata_upload <- max(plati_reactive$file_read_prel$DataPlata1,na.rm=TRUE) %>%
          lubridate::ceiling_date(unit = "month") - 1})
    
    # Observer for plati file input
    output$show_plati_date <- renderUI({req(input$plati_input)
      if (!plati_reactive$all_names) {
        
        paste("Lipseste coloana: ",plati_reactive$missing_names,collapse = " ; ")    }
      
      
      else if(!is.na(plati_reactive$data_plata_upload) | length(plati_reactive$data_plata_upload) !=0) {
        dateInput(inputId = session$ns("data_plati_input"),
                  label = "Selecteaza data raportului uploadat",
                  value = plati_reactive$data_plata_upload,autoclose = TRUE) }
      
      else {paste("Am intampinat probleme cu coloana DataPlata1. Contacteaza administratorul!!")}
    })
      
    output$output_save_plati <- renderUI({
        actionButton(inputId = session$ns("plati_input_save"),icon = icon("save"),
                     label = "Click aici pentru a salva fisierul uploadat")   })
    
    
    })
  
  observeEvent(input$data_plati_input,{
    
    plati_reactive$file_read_prel <- plati_reactive$file_read_prel %>% 
      dplyr::mutate(Plata_neta = PlatiEfective - TotalRecuperat, data_raport = input$data_plati_input) %>% 
      
      dplyr::mutate(Expunere_CTG_plati=ifelse(RecuperatCTG==0 & DataPlata1 > lubridate::`%m-%`(data_raport,months(24)),
                                              ProcCTG*Plata_neta/100,0))
    
    output$sumar_plati_input <- DT::renderDataTable({
      dt_generate_function(df = plati_reactive$file_read_prel %>% 
                             dplyr::summarise(Nr_contracte = dplyr::n(),PlatiBrute = sum(PlatiEfective), 
                             RecuperariTotale = sum(TotalRecuperat),  Garantii_accesorii_admise =  sum(ValoareAdmisaFNG),   
                             Expunere_CTG_plati = sum(Expunere_CTG_plati),
                             Provizioane_Constituite = sum(ProvizionNou),  
                             CreanteNete = PlatiBrute - RecuperariTotale,
                            Grad_Acoperire_Provizioane =  Provizioane_Constituite/CreanteNete) %>% 
                             dplyr::mutate(data_raport=input$data_plati_input) %>% dplyr::select(9,1:8), 
                           round_col = 2:8,perc_col = 9, 
                           caption = "Sinteza fisier uploadat:") %>%  
        DT::formatDate(columns = 1,method="toLocaleDateString")  })
  })
  
  observeEvent(input$plati_input_save,{
    removeUI("#database_upload_plati_ui_1-plati_input_save")
    if (input$data_plati_input <= threshold_date_plati_input) {
      shinyWidgets::sendSweetAlert(session = session,title = "STOP",text = "Nu pot salva fisiere anterioare datei de 01 Ianuarie 2019",
                                   type = "error",showCloseButton = TRUE)  }
    else {
     
      load('data/baza_provizioane_plati.rda')
      
      database_plati_reactive <- reactiveValues(df_old = baza_provizioane_plati, df_new = plati_reactive$file_read_prel,
              element_id = input$data_plati_input, column_id = "data_raport", finalise_process_compare_df = FALSE)
      
      
      
      callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = database_plati_reactive)
      
      observe({req(database_plati_reactive$finalise_process_compare_df)
        
        plati_reactive$sumar_plati <- database_plati_reactive$df_new_prel %>% dplyr::group_by(data_raport) %>%
          dplyr::summarise(Nr_contracte = dplyr::n(),PlatiBrute = sum(PlatiEfective), 
                           RecuperariTotale = sum(TotalRecuperat),  Garantii_accesorii_admise =  sum(ValoareAdmisaFNG),   
                           Expunere_CTG_plati = sum(Expunere_CTG_plati),
                           Provizioane_Constituite = sum(ProvizionNou),  
                           CreanteNete = PlatiBrute - RecuperariTotale,
                           Grad_Acoperire_Provizioane =  Provizioane_Constituite/CreanteNete)
        
        view_sumar_plati <- isolate(plati_reactive$sumar_plati)
        usethis::use_data(view_sumar_plati,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
        
        baza_provizioane_plati <- isolate(database_plati_reactive$df_new_prel)
        usethis::use_data(baza_provizioane_plati,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
        
        updateDateInput(session = session,inputId = "from_plati", value = tail(plati_reactive$sumar_plati$data_raport,2)[1])
        updateDateInput(session = session,inputId = "to_plati", value = tail(plati_reactive$sumar_plati$data_raport,1))
        })
      
      
    
    } 
    
    
  })
  
  observeEvent(input$delete_plati,{
    load('data/baza_provizioane_plati.rda')
    
    updateActionButton(session = session,inputId = "delete_plati")
    
    shinyWidgets::inputSweetAlert(session = session,input = "select",inputId = session$ns("date_delete_plati"),
        type = "warning", btn_colors = "#f3d512",btn_labels = "OK",
        inputOptions =  plati_reactive$sumar_plati$data_raport %>% unique(),
              title = "Selecteaza data raportului pe care vrei sa-l stergi")
  
    observeEvent(input$date_delete_plati,{
      if (as.Date.numeric(as.numeric(input$date_delete_plati),origin = "1970-01-01") <= threshold_date_plati_input) 
        
      {shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                    text = "Nu ai voie sa stergi observatii anterioare datei de 01 ianuarie 2019")}
      else {
        
          load('data/baza_provizioane_plati.rda')
          
          plati_reactive$provizioane_after_deletion <-   baza_provizioane_plati %>% 
            dplyr::filter(data_raport != as.Date.numeric(as.numeric(input$date_delete_plati),origin = "1970-01-01"))
          
          plati_reactive$sumar_plati <- plati_reactive$provizioane_after_deletion %>% dplyr::group_by(data_raport) %>%
            dplyr::summarise(Nr_contracte = dplyr::n(),PlatiBrute = sum(PlatiEfective), 
                             RecuperariTotale = sum(TotalRecuperat),  Garantii_accesorii_admise =  sum(ValoareAdmisaFNG),   
                             Expunere_CTG_plati = sum(Expunere_CTG_plati),
                             Provizioane_Constituite = sum(ProvizionNou),  
                             CreanteNete = PlatiBrute - RecuperariTotale,
                             Grad_Acoperire_Provizioane =  Provizioane_Constituite/CreanteNete)
          
          view_sumar_plati <- isolate(plati_reactive$sumar_plati)
          
          usethis::use_data(view_sumar_plati,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
          
          baza_provizioane_plati <- isolate(plati_reactive$provizioane_after_deletion)
          
          usethis::use_data(baza_provizioane_plati,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
         
          updateDateInput(session = session,inputId = "from_plati", value = tail(plati_reactive$sumar_plati$data_raport,2)[1])
          updateDateInput(session = session,inputId = "to_plati", value = tail(plati_reactive$sumar_plati$data_raport,1)) 
        }
    })
    
  
  
  })
  
  observeEvent(input$balanta_parteneri_input,{
    # I remove save button since plati_reactive$file_read_prel will no longer exists after module read excel will be called
    removeUI("#database_upload_plati_ui_1-plati_input_save")
    
    # I isolate plati_reactive$file_read_prel pentru ca voi mai avea nevoie de el si dupa ce dispare in urma call-ului catre modul read excel
    fisier_plati_input <- isolate(plati_reactive$file_read_prel)
    
    nume_obligatorii_balanta <- c("Partener|Cod","Partener|Denumire","Sold final|Debit","Sold final|Credit")
    
    balanta_reactiv <- reactiveValues(nume_obligatorii = nume_obligatorii_balanta)
    
    balanta_reactiv$file_input <- input$balanta_parteneri_input$datapath
    
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive = balanta_reactiv)
    
    observe({req(balanta_reactiv$all_names==TRUE)
      balanta_reactiv$prelucrata <- balanta_reactiv$file_read_prel %>% dplyr::filter(!is.na(`Partener|Cod`)) %>%
        dplyr::mutate(Sold_debitor = `Sold final|Debit` - `Sold final|Credit`) %>%
        dplyr::group_by(`Partener|Cod`,`Partener|Denumire`) %>%
        dplyr::summarise(Sold_final_debitor = sum(Sold_debitor)) %>% dplyr::ungroup()
      
      balanta_reactiv$fisier_input_prel <- fisier_plati_input  %>% dplyr::mutate(Plata_neta = PlatiEfective - TotalRecuperat) %>%
          dplyr::group_by(CodBeneficar) %>% dplyr::summarise(Plata_neta_Charisma = sum(Plata_neta))
      
      output$tabel_diferente <- DT::renderDataTable({
        dt_generate_function(df =  balanta_reactiv$fisier_input_prel %>% 
                dplyr::left_join(balanta_reactiv$prelucrata,  by = c("CodBeneficar" = "Partener|Cod")) %>% 
                dplyr::mutate(Diferente = Plata_neta_Charisma - Sold_final_debitor) %>% 
                  dplyr::filter(abs(Diferente) > 1 | is.na(Diferente)) %>%
                  dplyr::bind_rows(dplyr::left_join(x =  balanta_reactiv$prelucrata, y = balanta_reactiv$fisier_input_prel,
                                     by = c("Partener|Cod" = "CodBeneficar")) %>% 
                                     dplyr::mutate(Diferente = Plata_neta_Charisma - Sold_final_debitor) %>% 
                                     dplyr::filter(abs(Diferente) > 1 | is.na(Diferente))) %>%
                  # below i remove plata_neta_charisma = 0 si beneficiar inexistent in balanta
                  dplyr::filter(!(Plata_neta_Charisma == 0 & is.na(`Partener|Cod`))) %>%
                  dplyr::select(6,3,2,4,5,1),round_col = 3:5,caption = "Diferente identificate"
                )
      })
          
      
    })
    
  })
  
  
  
 
  
  
  
  
  
  
  
  
  
}
    
## To be copied in the UI
# mod_database_upload_plati_ui("database_upload_plati_ui_1")
    
## To be copied in the server
# callModule(mod_database_upload_plati_server, "database_upload_plati_ui_1")
 
