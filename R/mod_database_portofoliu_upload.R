#' database_portofoliu_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_database_portofoliu_upload_ui <- function(id){
  ns <- NS(id)
  shinydashboard::box( title = "Upload portfolio file",status = "success",width = 12,collapsible = TRUE,collapsed = FALSE,
                      fillRow(flex=c(1,NA),fileInput(ns("sold"),label = "Upload fisierul de solduri",  accept = c(".xlsx",".xls"),
                                    buttonLabel = "Excel Only",placeholder = "No file uploaded",width = "300px"), br(),
                              #verbatimTextOutput(ns("diverse_upload")),
                               uiOutput(outputId = ns("date_portof_output"))),br(),br(),br(), 
                       DT::dataTableOutput(ns("sumar_portof_input")), br(), uiOutput(ns("calculate_depreciate_output")),
                       DT::dataTableOutput(ns("coeficienti_provizioane_nonifrs")),br(),
                       uiOutput(ns("calculate_provizioane_depreciate_output")),
                       DT::dataTableOutput(ns("sinteza_portof_input_categorie")), br(),
                       uiOutput(ns("save_portofoliu_output")), br(),
                      DT::dataTableOutput(outputId = ns("provizion_detaliat")),
                      verbatimTextOutput(ns("check_proviz_updatat")),verbatimTextOutput(ns("check_portofoliu")))
                       
}
    
#' database_portofoliu_upload Server Function
#'
#' @noRd 
mod_database_portofoliu_upload_server <- function(input, output, session){
  ns <- session$ns
  
  #This module does not feed any reactive values to other modules. 
  
  nume_obligatorii= c("ID Document",'Beneficiar','Cod Partener','Banca','Nr contract','Valuta','Soldul garantiei [in LEI]',"Data contract",
                      'Soldul creditului [in LEI]','Procentul de garantare','Tip Fond [centralizat]')
  
  threshold_date_input <- as.Date("2018-12-31")
  
  vals <- reactiveValues() # I use it for coef_non_ifrs - less code to handle input inside datatable
  
  
  observeEvent(input$sold,{
    sheets_read <- reactive ({  shiny::validate(shiny::need(tools::file_ext(input$sold$datapath) %in% c("xlsx","xls"),
                                                            message = FALSE))
      readxl::excel_sheets(input$sold$datapath) })
    
    if (length(sheets_read())>1) {
      ns <- session$ns
      shinyWidgets::inputSweetAlert(session = session,inputId = ns("ok_sheet"), input = "select",inputOptions = sheets_read(),
          type = "warning",btn_colors = "#f3d512",btn_labels = "OK",
            title = "STOP, fiserul ure mai multe sheet-uri", "Selecteaza de mai jos sheet-ul pe care sa-l citesc") 
      #selected_sheet <- eventReactive(eventExpr = input$ok_sheet,{input$ok_sheet}) 
      observeEvent(input$ok_sheet,{vals$selected_sheet <- input$ok_sheet})
      }
    
      else if (length(sheets_read())==1) {
      vals$selected_sheet <-  readxl::excel_sheets(input$sold$datapath) }
    
 
    
    # First read of the portfolio
    
    #output$diverse_upload <- renderPrint({is.null(vals$selected_sheet)})
    
    
    portofoliu_first_read <- reactive({ req(vals$selected_sheet)
      readxl::read_excel(input$sold$datapath,sheet = vals$selected_sheet,n_max = 50) })
    
    
    # I get the row index where name if the columns are
    index_citire <- reactive({ req(portofoliu_first_read())
      apply(portofoliu_first_read(),1,function(x) (sum(nume_obligatorii %in% x)>=2)) %>% which(TRUE) })
    
    
    
    # Second read of the portofolio, this time starting where the column names are
    portofoliu_second_read <- reactive({ #req(index_citire())
      readxl::read_excel(input$sold$datapath,sheet = vals$selected_sheet, skip = max(index_citire(),0,na.rm = T)) %>%
      dplyr::mutate_at(.vars = c('Soldul garantiei [in LEI]','Procentul de garantare','ID Document'),
                       .funs = as.numeric) %>% dplyr::mutate_at(.vars = 'Cod Partener',.funs = as.character)  })
    
   
    # Check to see if I have the column SoldGarantieContragarantata_LEI or %contragarantare
    exista_proc_ctg <- reactive ({ "%contragarantare" %in% names(portofoliu_second_read()) })
    exista_sold_ctg <- reactive ({ "SoldGarantieContragarantata_LEI" %in% names(portofoliu_second_read()) })
    
    nu_exista_ctg <- reactive ({ if(!exista_sold_ctg() & !exista_proc_ctg()) {TRUE} 
      else {FALSE}   })
    
    sunt_nume_lipsa <- reactive ({ nume_obligatorii %in% names(portofoliu_second_read()) %>% all() })
    
    nume_lipsa <- reactive ({ setdiff(nume_obligatorii,names(portofoliu_second_read())) })
    
    # I get the date of the portofolio. It will be used as starting date for UI date input portfolio.
    # Validation message with column names missing will appear in date portfolio input
    
    date_upload_file <- reactive({shiny::validate(shiny::need(tools::file_ext(input$sold$datapath)=="xlsx",
            message = paste0("XLSX only! You uploaded a ",tools::file_ext(input$sold$datapath)," file")))
      
      shiny::validate(shiny::need(sunt_nume_lipsa(),message = paste0("Lipseste coloana: ",nume_lipsa(),collapse = ";") %>% stringr::str_c()))
      
      max(as.Date(portofoliu_second_read()$`Data contract`))    })
    
    # I process portfolio for contragarantii
    portofoliu_contragarantii <- reactive ({ shiny::validate(shiny::need(!nu_exista_ctg(),message = FALSE))
      if (exista_sold_ctg()) {portofoliu_second_read() %>% 
          dplyr::mutate(contragarantii = SoldGarantieContragarantata_LEI) %>% tidyr::replace_na(list(contragarantii=0)) }
      else if (exista_proc_ctg()) {portofoliu_second_read() %>% 
          dplyr::mutate(contragarantii=`%contragarantare` * `Soldul garantiei [in LEI]`/100) %>% tidyr::replace_na(list(contragarantii=0)) }
      else {portofoliu_second_read()}
    })
    
    
    output$date_portof_output <- renderUI({req(date_upload_file())
      ns <- session$ns
      dateInput(inputId = ns('date_portof_input'),label = "Selecteaza data portofoliului uploadat",
                value = date_upload_file(),min = as.Date("2001-12-31"),max = as.Date("2030-12-31"),width = "300px")
    })
    
    portofoliu_surse_proprii <- reactive({portofoliu_contragarantii() %>% 
        dplyr::filter(`Tip Fond [centralizat]` == sort(unique(portofoliu_contragarantii()$`Tip Fond [centralizat]`))[1]) })
   
    output$sumar_portof_input <- DT::renderDataTable({req(input$date_portof_input)
      dt_generate_function(round_col = 2:4,caption = "Sumar portofoliu uploadat:",
            df = portofoliu_surse_proprii() %>% dplyr::summarise(Nr_contracte = dplyr::n(),Sold_garantii = sum(`Soldul garantiei [in LEI]`),
              Sold_contragarantii=sum(contragarantii)) %>% dplyr::mutate(Data_raport_uploadat = as.character(input$date_portof_input)) %>% 
                             dplyr::select(4,1,2,3))   })
    
    output$calculate_depreciate_output <- renderUI({req(portofoliu_surse_proprii())
      ns <- session$ns
      actionButton(inputId = ns("calculate_depreciate"),label = "Calculeaza garantiile depreciate",icon = icon("chart-area"),
                   width = "300px")   })
    
    
    observeEvent(input$calculate_depreciate,{
      
      bi_sinteza <- readRDS("R/reactivedata/bi_sinteza.rds")
      
      removeUI("#database_portofoliu_upload_ui_1-calculate_depreciate")
      
      
      if (bi_sinteza$Snapshot_cereri_plata <= input$date_portof_input) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP, nu am actualizat BI-ul cu cereri de plata",type = "error",showCloseButton = TRUE,
                                     text = paste0("Eu am snapshot-ul ",as.character(bi_sinteza$Snapshot_cereri_plata)," Actualizeaza BI-ul cereri de plata
                    in fereastra de mai jos si apoi incearca din nou pentru a putea genera categoriile depreciate!"))  }
      
      else if (bi_sinteza$Snapshot_instiintari_neplata <= input$date_portof_input) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP,Nu am actualizat BI-ul instiintari de neplata pentru data raportului selectata
                        de tine",type = "error", showCloseButton = TRUE,
                                     text = paste0("Eu am snapshot-ul ",as.character(bi_sinteza$Snapshot_instiintari_neplata),
                                                   " Actualizeaza BI-ul cu instiintari de neplata in fereastra de mai jos si apoi incearca din nou pentru a putea genera categoriile depreciate!")) }
      
      else {
        baza_cereri_plata <- readRDS("R/reactivedata/bi_cereri_plata.rds")
        baza_instiintari <- readRDS("R/reactivedata/bi_instiintari.rds")
        baza_insolventa <- readRDS("R/reactivedata/insolventa.rds")
        
        portofoliu_cereri_plata <- reactive({ portofoliu_surse_proprii() %>% dplyr::left_join(y = baza_cereri_plata, by = 'ID Document') })
        portofoliu_instiintari <- reactive({portofoliu_cereri_plata() %>% dplyr::left_join(y = baza_instiintari,by = 'ID Document') })
        portofoliu_prelucrat <- reactive({portofoliu_instiintari() %>% dplyr::left_join(y = baza_insolventa, by=c('Cod Partener'='Cod'))  })
        
       
        
        if (max(baza_insolventa$DataInsolventa) < input$date_portof_input) {
          shinyWidgets::sendSweetAlert(session=session,title = "ATENTIE",type = "warning",showCloseButton = TRUE,
                                       text = paste0("Atentie, data maxima de insolventa pe care o am stocata ,", as.character(max(baza_insolventa$DataInsolventa)),
                                                     ", este mai mica decat data raportului selectata de tine Acesta este doar un mesaj de atentionare, voi merge mai departe 
                    cu prelucrarea portofoliului. Poti updatat fisierul de insolvente in ferereastra de mai jos.")) } 
        
    
    portofoliu_categorie <- eventReactive(input$date_portof_input,{
      portofoliu_prelucrat() %>% dplyr::mutate(anul_de_raportare = input$date_portof_input) %>% 
        dplyr::group_by(`Cod Partener`, Beneficiar,anul_de_raportare) %>% 
        dplyr::summarise(expunere = sum(`Soldul garantiei [in LEI]`), contragarantii = sum(contragarantii),
                         min_cerere = ifelse(all(is.na(data_cerere_plata)), NA, min(data_cerere_plata, na.rm = TRUE)),
                         min_insolventa = ifelse(all(is.na(DataInsolventa)), NA, min(DataInsolventa, na.rm = TRUE)),
                         min_instiintare = ifelse(all(is.na(data_instiintare)), NA, min(data_instiintare, na.rm = TRUE))) %>%
        dplyr::mutate_at(.vars = dplyr::vars(dplyr::matches("min_")),  as.Date.numeric, origin = "1970-01-01") %>% as.data.frame() %>%
        dplyr::mutate(categorie_contaminata = ifelse(!is.na(min_cerere) &  min_cerere <= anul_de_raportare,"cerere_plata",
                                ifelse(!is.na(min_insolventa) & min_insolventa <= anul_de_raportare,"insolventa",
                          ifelse(!is.na(min_instiintare) & min_instiintare <= anul_de_raportare,"instiintare_neplata",  "standard")  )) ) })
      
    output$sinteza_portof_input_categorie <- DT::renderDataTable({req(portofoliu_categorie())
        dt_generate_function(df = portofoliu_categorie() %>% dplyr::group_by(categorie_contaminata) %>% 
                               dplyr::summarise(expunere=sum(expunere), contragarantii=sum(contragarantii)) %>% 
                               dplyr::bind_rows(apply(X = dplyr::select(.,-1),MARGIN = 2,FUN=sum)) %>% 
                               tidyr::replace_na(replace = list(categorie_contaminata="Total")),
                             caption = paste0("Sinteza portofoliu depreciat la data de ",input$date_portof_input," :"),round_col = 2:3)   })
      
    vals$coef_non_ifrs <- readRDS(file = "data/coef_non_ifrs.rds")
    output$coeficienti_provizioane_nonifrs <- DT::renderDataTable({
        dt_generate_function(df = vals$coef_non_ifrs,editable="cell",round_col = 1:5,digits = 2,
                             caption = "Voi folosi coeficientii de mai jos pentru a genera provizioanele. Dublu-clik in oricare din celule 
              entru a edita coeficientul iar valoarea provizonului se va actualza corespunzator si in tabelele de mai sus.")  })
    
    output$calculate_provizioane_depreciate_output <- renderUI({
        ns <- session$ns
        actionButton(inputId = ns("calculate_provizioane_depreciate"),label = "Calculeaza provizioanele depreciate",width = "300px")
      })
      }
      
  
    observeEvent(eventExpr = input[['coeficienti_provizioane_nonifrs_cell_edit']],{
      cellinfo <-  input[['coeficienti_provizioane_nonifrs_cell_edit']]
      vals$coef_non_ifrs <- DT::editData(data = vals$coef_non_ifrs, info=cellinfo,rownames = FALSE) 
      
      output$calculate_provizioane_depreciate_output <- renderUI({
        ns <- session$ns
        actionButton(inputId = ns("calculate_provizioane_depreciate"),label = "Recalculeaza provizioanele",width = "300px")
      })
      
    })
    
    observeEvent(input$calculate_provizioane_depreciate,{
      
      removeUI("#database_portofoliu_upload_ui_1-calculate_provizioane_depreciate")
      
      output$save_portofoliu_output <- renderUI({
        
        fillRow(flex = c(1,NA),actionLink(inputId = ns("link_portofoliu"),label = "Click aici pentru a vedea si modifica portofoliul"),
                actionButton(inputId = session$ns("save_portofoliu"),label = "Salveaza portofoliul prelucrat",width = "300px"))  })
      
      
      portofoliu_provizion <- reactive({ dplyr::left_join(x = portofoliu_prelucrat(),    y = dplyr::select(portofoliu_categorie(),
              'Cod Partener', categorie_contaminata,   anul_de_raportare),by = "Cod Partener") %>%
        dplyr::select(DocumentId = 'ID Document', anul_de_raportare, Banca,Beneficiar, 'Cod Partener',
                      'Nr contract',expunere = 'Soldul garantiei [in LEI]',   contragarantii,
                      categorie_contaminata) %>% 
        dplyr::mutate(provizion_contabil = ifelse(
          categorie_contaminata == "cerere_plata",
          (expunere - contragarantii * vals$coef_non_ifrs[1,5]) * vals$coef_non_ifrs[1,3] * vals$coef_non_ifrs[1,4],
          ifelse(categorie_contaminata == "insolventa",
                 (expunere - contragarantii * vals$coef_non_ifrs[1,5]) * vals$coef_non_ifrs[1,1] * vals$coef_non_ifrs[1,3] * vals$coef_non_ifrs[1,4],
                 ifelse(categorie_contaminata == "instiintare_neplata",
                        (expunere - contragarantii * vals$coef_non_ifrs[1,5]) * vals$coef_non_ifrs[1,2] * vals$coef_non_ifrs[1,3] * vals$coef_non_ifrs[1,4],
                        0)))) })
      
      output$provizion_detaliat <- DT::renderDataTable({req(input$link_portofoliu)
        DT::datatable(data = portofoliu_provizion(),rownames = FALSE,editable = list(target='cell',
                        disable=list(columns=0:7)),caption = "Double click in ultimele 2 coloane pentru a modifica. 
                      Foloseste search. Atentie, apasand butonul save vei salva valorile completate ale provizionului") %>%
                DT::formatRound(columns = c(7,8,10), digits = 2)  })
      
      portofoliu_provizion_updatat <- eventReactive(input[['provizion_detaliat_cell_edit']],{
        DT::editData(portofoliu_provizion(), info=input[['provizion_detaliat_cell_edit']],rownames = FALSE) })
      
      output$sinteza_portof_input_categorie <- DT::renderDataTable({
      dt_generate_function(caption = paste0("Sinteza portofoliu depreciat si provizioane la data de ",input$date_portof_input," :"),
            round_col = 2:4,  df = portofoliu_provizion() %>% dplyr::group_by(categorie_contaminata) %>% 
              dplyr::summarise(expunere=sum(expunere),contragarantii=sum(contragarantii),provizion=sum(provizion_contabil)) %>% 
           dplyr::bind_rows(apply(X = dplyr::select(.,-1),MARGIN = 2,FUN=sum)) %>% 
            tidyr::replace_na(replace = list(categorie_contaminata="Total")))    })
      
      
      
      observeEvent(input$save_portofoliu,{
        
        withProgress(expr = {
        portof_database <- readRDS('R/reactivedata/portof_database.rds')
        dates_portof_database <- portof_database %>% dplyr::pull(var = anul_de_raportare) %>% unique() %>% sort()
        
        removeUI("#database_portofoliu_upload_ui_1-save_portofoliu")
        removeUI("#database_portofoliu_upload_ui_1-sumar_portof_input")
        removeUI("#database_portofoliu_upload_ui_1-coeficienti_provizioane_nonifrs")
        removeUI("#database_portofoliu_upload_ui_1-date_portof_input")
        removeUI("#database_portofoliu_upload_ui_1-provizion_detaliat")
        removeUI("#database_portofoliu_upload_ui_1-link_portofoliu")
        
        check_df_type <- reactive({ janitor::compare_df_cols_same(portof_database,portofoliu_provizion(),
                                                            bind_method = "rbind",verbose = FALSE)  })
        
        if (input$date_portof_input <= threshold_date_input) {
          shinyWidgets::sendSweetAlert(session = session,title = "STOP",text = "Nu pot salva fisiere anterioare datei de 01 Ianuarie 2019",
                                       type = "error",showCloseButton = TRUE)  }
        
        else if (as.character(input$date_portof_input) %in% as.character(dates_portof_database)) {
          shinyWidgets::inputSweetAlert(session = session,inputId = "overwrite_portof_input",input = "radio",
                                        inputOptions = c("Overwrite?","Renunta!"),
                                        title = "ATENTIE",type = "warning",showCloseButton = FALSE,
                                        text = "Am deja un fisier cu data selectata de tine in baza mea de date")  
          
          observeEvent(input$overwrite_portof_input,{
            
            if (input$overwrite_portof_input=="Renunta!"){
              shinyWidgets::closeSweetAlert(session = session)}    
            
            else if (check_df_type()) {
              
              portof_database_filtered <- reactive({dplyr::filter(portof_database,anul_de_raportare != input$date_portof_input)})
                                            
              if (is.null(input[['coeficienti_provizioane_nonifrs_cell_edit']])){
              saveRDS(object = rbind(portof_database_filtered(), portofoliu_provizion()),  file = "R/reactivedata/portof_database.rds")
              
              shinyWidgets::sendSweetAlert(session = session,title = "SUCCES", type ="success",showCloseButton = TRUE,
                                           text = "Baza de date a fost uploadata cu succes") }
              else {saveRDS(object = rbind(portof_database_filtered(), portofoliu_provizion_updatat()),  file = "R/reactivedata/portof_database.rds")
                
                shinyWidgets::sendSweetAlert(session = session,title = "SUCCES", type ="success",showCloseButton = TRUE,
                            text = "Baza de date a fost uploadata cu succes folosind datele completate de tine")}
              
            }
          })
          }
        
        else if (check_df_type()) {
          if (is.null(input[['coeficienti_provizioane_nonifrs_cell_edit']])) {
            saveRDS(object = rbind(portof_database,portofoliu_provizion()),file = "R/reactivedata/portof_database.rds",
                    version = 3,compress = "gzip")
            
            shinyWidgets::sendSweetAlert(session = session,title = "SUCCES",text = "Am salvat cu succes",
                                         type = "success",showCloseButton = TRUE) }
          else {
            saveRDS(object = rbind(portof_database,portofoliu_provizion_updatat()),file = "R/reactivedata/portof_database.rds",
                    version = 3,compress = "gzip")
            
            shinyWidgets::sendSweetAlert(session = session,title = "SUCCES",text = "Am salvat cu succes folosind datele completate de tine.",
                                         type = "success",showCloseButton = TRUE)
          }
          }
        },message = "Saving portofoliu")
      })
    
  })
    
  })
    
    })
  
  
      
    
  
}
    
## To be copied in the UI
# mod_database_portofoliu_upload_ui("database_portofoliu_upload_ui_1")
    
## To be copied in the server
# callModule(mod_database_portofoliu_upload_server, "database_portofoliu_upload_ui_1")
 
