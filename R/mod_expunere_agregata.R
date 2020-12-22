#' expunere_agregata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_expunere_agregata_ui <- function(id)  {
  ns <- NS(id)
  
  shinydashboard::tabBox(width = 12,tabPanel(title = "Upload",icon = icon("upload"),
  
  shinyjs::useShinyjs(),
 
  shinybusy::add_busy_spinner(color = "#77547a", position = "bottom-right", timeout = 200),
                    
  fluidRow(
    # Box upload CUI
    shinydashboard::box(title = "Gestiune BI: CUI, Document ID, Cod Partener" , width = 12,status = "success",
                  collapsible = TRUE,collapsed = TRUE,
              footer = "Se downloadeaza BI-ul folosind link-ul si se actualizeaza data snapshot in ambele sheet-uri",
    div(id = ns("welcome_cui"),h4("Este recomandat ca procesul de constructie a expunerii agregate sa inceapa cu acest pas din care
            se vor extrage CUI-urile necesare.")),
    verbatimTextOutput(ns("diverse")),
    DT::dataTableOutput(ns("sumar_bi_cui")), br(),
    fluidRow(
    column(width =6,fileInput(inputId = ns("upload_bi_cui"),label = "Upload BI - cui-uri:",accept = c(".xlsx",".xls"),width = "300px",
              buttonLabel = "Excel only",placeholder = "no file uploaded")), 
    column(width=6,br(),downloadLink(outputId = ns("link_bi_cui"),label = "Descarca BI-ul pentru a actualiza snapshot-ul")),
    
    DT::dataTableOutput(ns("sumar_bi_cui_upload")), br(),
    uiOutput(ns("show_save_cui"))) ),
  
  # Box upload portofoliu  
  shinydashboard::box(title = "Upload portofoliu",width = 12,status = "success",collapsible = TRUE,collapsed = FALSE,
        footer = "Se uploadeaza fisierul de pe Fileserver - Provizioane",
  fluidRow(  
  column(width=6,fileInput(inputId = ns("portofoliu_expunere"),label = "Upload portofoliu file",accept = c(".xlsx",".xls"),width = "300px",
            buttonLabel = "Excel only",placeholder = "no file uploaded")),
  column(width=6,uiOutput(ns("portofoliu_error_show_sumar_expunere")))),
  
  tableOutput(ns("sumar_expunere"))),
  
  # Box upload plati
  shinydashboard::box(title = "Upload plati. Dupa ce se uploadeaza fisa contului 2821.4 va aparea butonul de upload al fisei contului 2911.4",
      width = 12,status = "success",collapsible = TRUE,collapsed = FALSE,
    footer = "Se uploadeaza fisierele de la contabilitate, respectiv fisele conturilor 2821.4 si 2911.4",
    
    fluidRow(
    column(width=6,fileInput(inputId = ns("contabilitate_28214"),label = "Upload fisierul 2821.4",accept = c(".xlsx",".xls"),width = "300px",
            buttonLabel = "Excel only",placeholder = "no file uploaded")),
    column(width=6, textOutput(ns("contabilitate_28214_error"))) ),
    
    tableOutput(ns("sumar_contabilitate_2821_4")),
  
    uiOutput(ns("show_file_2911"))),
  
  # Box comisioane finantari  
  shinydashboard::box(title = "Upload comisioane finantari",
                      width = 12,status = "success",collapsible = TRUE,collapsed = FALSE,
            footer = "Se uploadeaza fisierul de la contabilitate, respectiv fisa contului 382101",
  fluidRow(
  column(width=6,fileInput(inputId = ns("contabilitate_3821"),label = "Upload fisierul 3821",accept = c(".xlsx",".xls"),width = "300px",
                      buttonLabel = "Excel only",placeholder = "no file uploaded")),
  column(width=6,textOutput(ns("contabilitate_3821_error"))) ),
  
  tableOutput(ns("sumar_contabilitate_3821"))),
  
  # Box titluri
  shinydashboard::box(title = "Completeaza valoarea titlurilor",
                      width = 12,status = "success",collapsible = TRUE,collapsed = FALSE,
                      footer = "Se completeaza valoarea titlurilor de stat",
                      fluidRow(column(width = 6, numericInput(inputId = ns("valoare_titluri"),value = 0,min = 0,width = "300px",
                                                              label = "Introdu mai jos valoarea titlurilor de stat:")),
                               column(width = 6, br(),actionButton(inputId = ns("action_titluri"),icon=icon("step-forward"),width="320px",
                                     label = "Trimite valoarea titlurilor in expunerea agregata")))),
  
  # Box comisioane neincasate
  shinydashboard::box(title = "Upload comisioane neincasate",
                      width = 12,status = "success",collapsible = TRUE,collapsed = FALSE,
                      footer = "Se uploadeaza fisierul de la contabilitate - componenta sold clienti 3781_3811_3812",
  fluidRow(
    column(width=6,
          fileInput(inputId = ns("contabilitate_comisioane"),label = "Upload comisioane neincasate",
              accept = c(".xlsx",".xls"),width = "300px", buttonLabel = "Excel only",placeholder = "no file uploaded")),
          column(width=6,tableOutput(ns("sinteza_upload_comisioane")))),
  
  
  fluidRow(column(width = 6,tableOutput(ns("sumar_contabilitate_comisioane"))),
          column(width= 6,tableOutput(ns("contabilitate_comisioane_excluse")))  ),
          column(width=6,tableOutput(ns("contabilitate_comisioane_diverse")))),
 
   
  )),
  
  # TabPanel Expunere Agregata
  tabPanel(title = "Expunere Agregata",icon = icon("pound-sign"),
      div(id = ns("welcome_expunere_agregata"),h4("Aici se va construi expunerea agregata pe masura ce sunt uploadate 
      fisiere necesare: portofoliul, fisierele de contabilitate precum si valoarea titlurilor.")), br(),
      shinyWidgets::progressBar(id = ns("progress_expunere_agregata"), value = 0,display_pct = TRUE,striped = TRUE,
              title = "Stadiu constructie expunere agregata",status = "success"),
      DT::dataTableOutput(ns("sumar_expunere_agregata")), br(),
      uiOutput(ns("show_expunere_agreata")),br(),
      DT::dataTableOutput(ns("expunere_agregata_detaliata")),br(), br(),
      #verbatimTextOutput(ns("diverse")),
      column(width=12,uiOutput(ns("show_button_cui"))),
      fluidRow(column(width = 7, DT::dataTableOutput(ns("expunere_agregata_CUI_lipsa"))),
               column(width=5, shinyjs::hidden(DT::dataTableOutput(ns("additional_cui_data"))))),
     
     fluidRow(
      column(width=6, DT::dataTableOutput(ns("expunere_agregata_nume_dublat"))),
      column(width=6, DT::dataTableOutput(ns("expunere_agregata_cod_partener_dublat"))))
      )
  )
  
}
    
#' expunere_agregata Server Function
#'
#' @noRd 
mod_expunere_agregata_server <- function(input, output, session){
  ns <- session$ns
  
  expunere_agregata <- reactiveValues(progress=0)
  
 load("data/document_id_cui.rda")
 load("data/cod_partener_cui.rda")
 load("data/baza_date_cui.rda")
 load("data/additional_cui_match.rda")
 
 output$diverse <- renderPrint({expunere_agregata$progress})
 
  output$link_bi_cui <- downloadHandler(filename = {"BI_cui.xlsx"}, content = function(file) {file.copy(from = "inst/extdata/bi_cui.xlsx",to = file)})
 
  output$sumar_bi_cui <- DT::renderDataTable({dt_generate_function(df = baza_date_cui,round_col = c(1,3),
          caption = "Sinteza baza de date existenta:")})
  
  observeEvent(expunere_agregata$progress,{shinyWidgets::updateProgressBar(session = session,
      id = "progress_expunere_agregata",  value = expunere_agregata$progress)  })
  
  observeEvent(input$upload_bi_cui,{
    removeUI("#expunere_agregata_ui_1-welcome_cui")
    if (!all(readxl::excel_sheets(input$upload_bi_cui$datapath) %in% c("cod_partener" , "document_id"))) {
     shinyWidgets::sendSweetAlert(title = "STOP",type = "error",showCloseButton = TRUE,closeOnClickOutside = TRUE,
                text = "Fisierul uploadat trebuie sa contina sheet-urile cod_partener si docuemnt_id")  }
   
   else {
     
     document_id_cui <- reactive({ readxl::read_excel(input$upload_bi_cui$datapath,sheet = "document_id",skip = 4) %>%
         dplyr::filter(!is.na(`Contract Id`)) %>% dplyr::mutate_all(as.character) %>% 
         dplyr::rename_at("Unique ID",~"CUI") })
     
     snapshot_id_cui <- reactive({readxl::read_excel(input$upload_bi_cui$datapath,sheet = "document_id",n_max = 6) %>% 
       dplyr::slice(1) %>% dplyr::pull(2) %>% as.Date() })
     
     cod_partener_cui <- reactive({ readxl::read_excel(input$upload_bi_cui$datapath,sheet = "cod_partener",skip = 4) %>%
       dplyr::filter(!is.na(Code)) %>% dplyr::mutate_all(as.character) %>%
         dplyr::rename_at("Unique ID",~"CUI")  })
     
     snapshot_cod_partener_cui <- reactive({readxl::read_excel(input$upload_bi_cui$datapath,sheet = "cod_partener",
          n_max = 6) %>%   dplyr::slice(1) %>% dplyr::pull(2) %>% as.Date() })
     
     sumar_bi_cui_upload <- reactive({data.frame(Nr_observatii_id_cui = nrow(document_id_cui()),Snapshot_id_cui = snapshot_id_cui(),
                                                 Nr_observatii_cod_partener_cui = nrow(cod_partener_cui()),Snapshot_cod_partener_cui = snapshot_cod_partener_cui(),
                                                 stringsAsFactors = FALSE) })
     
     output$sumar_bi_cui_upload <- DT::renderDataTable({ dt_generate_function(caption = "Sinteza upload:",
        round_col = c(1,3), df = sumar_bi_cui_upload()) })
     
     output$show_save_cui <- renderUI({req(document_id_cui(),cod_partener_cui())
       column(width = 6, actionButton(inputId = session$ns("save_cui"),icon = icon("save"),width = "300px",
                                      label = "Click aici pentru a salva datele uploadate"))
       })
     
     observeEvent(input$save_cui,{
       removeUI("#expunere_agregata_ui_1-save_cui")
       removeUI("#expunere_agregata_ui_1-sumar_bi_cui_upload")
       
       document_id_cui <- isolate(document_id_cui())
       cod_partener_cui <- isolate(cod_partener_cui())
       baza_date_cui <- isolate(sumar_bi_cui_upload())  
       
       usethis::use_data(document_id_cui,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
       usethis::use_data(cod_partener_cui,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
       usethis::use_data(baza_date_cui,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
       
       shinyWidgets::sendSweetAlert(session = session,title = "Succes",type = "success",showCloseButton = TRUE,
                                    text = "Baza de date s-a actualizat cu succes")
       
       output$sumar_bi_cui <- DT::renderDataTable({dt_generate_function(df = baza_date_cui,round_col = c(1,3),
                     caption = "Sinteza baza de date existenta:")})
       
     })
   }
   
 })
  
 
  observeEvent(input$portofoliu_expunere,{
    nume_obligatorii_portofoliu <- c("Cod Partener","Beneficiar","Soldul garantiei [in LEI]",
                        "Tip Fond [centralizat]","Tip fonduri","ID Document")
    
    expunere_reactiva <- reactiveValues(nume_obligatorii=nume_obligatorii_portofoliu)
    
    expunere_reactiva$file_input <- input$portofoliu_expunere$datapath
    
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive=expunere_reactiva)
    
    observeEvent(expunere_reactiva$all_names,{
      
      if (expunere_reactiva$all_names==FALSE) {
      output$portofoliu_error_show_sumar_expunere <- renderUI({
        paste("Lipseste coloana: ",expunere_reactiva$missing_names,collapse = " ; ")      })  
      }
    
      else if (expunere_reactiva$all_names==TRUE) {
      output$portofoliu_error_show_sumar_expunere <- renderUI({fluidRow( br(),
        div(class="pull-right",  actionButton(inputId = session$ns("action_show_expunere"),
              label = "Hide/Show distributia portofoliului de garantii uploadat",icon = icon("toggle")) ) )})
      
      observeEvent(input$action_show_expunere,{
        shinyjs::toggle("sumar_expunere") })
      
      expunere_reactiva$portofoliu <- expunere_reactiva$file_read_prel %>% 
        dplyr::mutate_at(c("ID Document","Cod Partener"),as.character) %>%
        dplyr::left_join(document_id_cui, by=c("ID Document" = "Contract Id"))
      
      isolate(expunere_agregata$final_report <- dplyr::bind_rows(expunere_reactiva$portofoliu %>% 
         dplyr::filter(`Tip Fond [centralizat]` %in% c("1. Fd. proprii","2. Garantii din surse MADR")) %>%
         dplyr::mutate(Expunere_Bruta = `Soldul garantiei [in LEI]`/2) %>%
         dplyr::filter(Expunere_Bruta >0) %>%
         dplyr::group_by(`Cod Partener`,CUI,Beneficiar) %>% dplyr::summarise(Expunere_Bruta = sum(Expunere_Bruta)) %>% 
         dplyr::ungroup() %>%
         dplyr::mutate(Tip_expunere="garantii") %>%
         dplyr::mutate(Grad_risc_contrapartida = 0.5) %>%
         dplyr::mutate(Tipologie_expunere = "elem. In afara bilantului") %>%
         dplyr::mutate(Expunere_Neta = Expunere_Bruta * Grad_risc_contrapartida),expunere_agregata$final_report)    ) 
      
      isolate(expunere_agregata$progress <- expunere_agregata$progress + 50)
      
      
      
      shinyjs::disable(id = "portofoliu_expunere")
      
      expunere_reactiva$sumar_portofoliu <- expunere_reactiva$portofoliu %>% 
        dplyr::mutate(Sursa_finantare = ifelse(is.na(`Tip Fond [centralizat]`),"necompletat",
                                               ifelse(stringr::str_detect(`Tip Fond [centralizat]`,pattern = "1."),"Surse_proprii",
                                                      ifelse(stringr::str_detect(`Tip Fond [centralizat]`,pattern = "2."),"Surse_administrare","Garantii_stat")))) %>%
        dplyr::group_by(Sursa_finantare,Tip_fonduri = `Tip fonduri`) %>%
        dplyr::summarise(Numar_Beneficiari = dplyr::n_distinct(`Cod Partener`),
                         "Sold_garantii"=sum(`Soldul garantiei [in LEI]`)) %>% 
        dplyr::bind_rows(data.frame('Sursa_finantare'= "Subtotal_surse_proprii_administrare","Tip_fonduri"='-',
                                    'Numar_Beneficiari' = dplyr::n_distinct(expunere_reactiva$portofoliu$`Cod Partener`[expunere_reactiva$portofoliu$`Tip Fond [centralizat]` %in% 
                                                                                                                          c("1. Fd. proprii","2. Garantii din surse MADR")]),
                                    'Sold_garantii' = sum(expunere_reactiva$portofoliu$`Soldul garantiei [in LEI]`[expunere_reactiva$portofoliu$`Tip Fond [centralizat]` %in% 
                                                                                                                     c("1. Fd. proprii","2. Garantii din surse MADR")]))) %>%
        dplyr::arrange(factor(Sursa_finantare,
                              levels = c("Surse_proprii","Surse_administrare","Subtotal_surse_proprii_administrare",
                                         "Garantii_stat","necompletat"))) %>% dplyr::ungroup() %>% 
        dplyr::mutate_at(.vars = c("Numar_Beneficiari","Sold_garantii"),formatC,format="f",digits=0,big.mark=",")
      
      output$sumar_expunere <- function() {
        expunere_reactiva$sumar_portofoliu %>%
          knitr::kable(format = "html",align = c("l","l","r","r"),caption = "Sinteza portofoliu uploadat:") %>% 
          kableExtra::collapse_rows() %>% 
          kableExtra::row_spec(row = stringr::str_which(string = expunere_reactiva$sumar_portofoliu$Sursa_finantare,
                                                        pattern="Subtotal_surse_proprii_administrare"),bold = TRUE) %>%
          kableExtra::kable_styling(bootstrap_options = c("condensed"))
      }
    }
    })
    
    
   
   
  
   
  })
  
  
  observeEvent(input$contabilitate_28214,{
    
    nume_obligatorii_contabilitate_2821_4 <- c("Partener|Cod","Sold final|Debit","Sold final|Credit","Partener|Denumire")

    
    expunere_reactiva <- reactiveValues(nume_obligatorii=nume_obligatorii_contabilitate_2821_4)
    
    expunere_reactiva$file_input <- input$contabilitate_28214$datapath
    
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive=expunere_reactiva)
    
    output$contabilitate_28214_error <- renderText({req(expunere_reactiva$all_names==FALSE)
      paste("Lipseste coloana: ",expunere_reactiva$missing_names, collapse = " ; ")      })
    
    observe({req(expunere_reactiva$all_names)
      
      expunere_reactiva$contabilitate_2821_4 <- expunere_reactiva$file_read_prel %>% 
        dplyr::filter(!is.na(`Partener|Cod`)) %>% dplyr::left_join(y = cod_partener_cui, by = c("Partener|Cod" = "Code")) 
      
      output$sumar_contabilitate_2821_4<- function() {
         expunere_reactiva$contabilitate_2821_4 %>% 
          dplyr::summarise(Numar_Beneficiari=dplyr::n_distinct(`Partener|Cod`),
                    Sold_debitor = sum(`Sold final|Debit`),Sold_creditor = sum(`Sold final|Credit`)) %>%
          dplyr::mutate_all(formatC,format="f",digits=0,big.mark=",") %>%
          knitr::kable(format = "html",align = c("r","r","r"),caption = "Sinteza contabilitate 2821_4:") %>% 
          kableExtra::kable_styling(bootstrap_options = c("condensed"))  }
      
      output$show_file_2911 <- renderUI({req(expunere_reactiva$contabilitate_2821_4)
        fluidRow(
        column(width=6,fileInput(inputId = session$ns("contabilitate_2911_4"),label = "Upload fisa cont 2911.4",accept = c(".xlsx",".xls"),
                  buttonLabel = "Excel only",placeholder = "no file uploaded",width = "300px")),
        column(width = 6, textOutput(ns("contabilitate_2911_4_error"))) ) })
      
      isolate(expunere_agregata$final_report <- dplyr::bind_rows(expunere_reactiva$contabilitate_2821_4 %>% 
            dplyr::select(`Cod Partener`= "Partener|Cod",Expunere_Bruta="Sold final|Debit",Beneficiar="Partener|Denumire",CUI) %>%
                          dplyr::mutate(Tip_expunere="plati") %>%
                          dplyr::mutate(Grad_risc_contrapartida = ifelse(Tip_expunere=="garantii",0.5,1)) %>%
                          dplyr::mutate(Tipologie_expunere = ifelse(Tip_expunere=="garantii",
                                                                    "elem. In afara bilantului","elem. De activ")) %>%
                          dplyr::mutate(Expunere_Neta = Expunere_Bruta * Grad_risc_contrapartida),
                        expunere_agregata$final_report))
      isolate(expunere_agregata$progress <- expunere_agregata$progress + 15)
     
      
      shinyjs::disable(id = "contabilitate_28214")
      })
    
  
  observeEvent(input$contabilitate_2911_4,{
    
    nume_obligatorii_contabilitate_2911_4 <- c("Partener|Cod","Sold final|Credit")
    
    
    expunere_reactiva <- reactiveValues(nume_obligatorii=nume_obligatorii_contabilitate_2911_4)
    
    expunere_reactiva$file_input <- input$contabilitate_2911_4$datapath
    
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive=expunere_reactiva)
    
    output$contabilitate_2911_4_error <- renderText({req(expunere_reactiva$all_names==FALSE)
      paste("Lipseste coloana: ",expunere_reactiva$missing_names, collapse = " ; ")      })
    
    observe({req(expunere_reactiva$all_names)
      
      expunere_reactiva$contabilitate_2911_4 <- expunere_reactiva$file_read_prel %>% 
        dplyr::filter(!is.na(`Partener|Cod`)) %>% dplyr::group_by(`Partener|Cod`) %>% 
        dplyr::summarise(`Sold final|Credit`=sum(`Sold final|Credit`)) %>% dplyr::ungroup()
      
      isolate(expunere_agregata$final_report <- dplyr::filter(expunere_agregata$final_report,Tip_expunere=="plati") %>% 
                dplyr::left_join(expunere_reactiva$contabilitate_2911_4,by = c(`Cod Partener` = "Partener|Cod")) %>% 
                tidyr::replace_na(list("Sold final|Credit" = 0)) %>%
                dplyr::mutate(Expunere_Bruta = Expunere_Bruta - `Sold final|Credit`) %>% dplyr::select(-`Sold final|Credit`) %>% 
                dplyr::filter(!is.na(Expunere_Bruta)) %>%
                dplyr::mutate(Grad_risc_contrapartida = ifelse(Tip_expunere=="garantii",0.5,1)) %>%
                dplyr::mutate(Tipologie_expunere = ifelse(Tip_expunere=="garantii",
                                                          "elem. In afara bilantului","elem. De activ")) %>%
                dplyr::mutate(Expunere_Neta = Expunere_Bruta * Grad_risc_contrapartida) %>% 
                dplyr::bind_rows(dplyr::filter(expunere_agregata$final_report,Tip_expunere!="plati"))  )
      
      isolate(expunere_agregata$progress <- expunere_agregata$progress + 15)
      
      
      output$sumar_contabilitate_2821_4 <- function() {
        dplyr::filter(expunere_agregata$final_report,Tip_expunere=="plati") %>% 
          dplyr::summarise(Nr_beneficiari = dplyr::n_distinct(`Cod Partener`),Expunere_Bruta = sum(Expunere_Bruta)) %>%
          dplyr::mutate_all(formatC,format="f",digits=0,big.mark=",") %>%
        #expunere_reactiva$contabilitate_2911_4 %>% 
         # dplyr::summarise(Numar_Beneficiari=dplyr::n_distinct(`Partener|Cod`),
           #                Sold_creditor = sum(`Sold final|Credit`)) %>%
          #dplyr::mutate_all(formatC,format="f",digits=0,big.mark=",") %>%
          knitr::kable(format = "html",align = c("r","r"),caption = "Sinteza contabilitate plati. 
          Am dedus soldul final creditor al contului 2911 din soldul final debitor al contului 2811:") %>% 
          kableExtra::kable_styling(bootstrap_options = c("condensed"))  }
      
     
    })
    
  })
  
  })
  
  
  observeEvent(input$contabilitate_3821,{
    
    nume_obligatorii_contabilitate_3821 <- c("Partener|Cod","Sold final|Debit","Partener|Denumire")
    
    
    expunere_reactiva <- reactiveValues(nume_obligatorii=nume_obligatorii_contabilitate_3821)
    
    expunere_reactiva$file_input <- input$contabilitate_3821$datapath
    
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive=expunere_reactiva)
    
    output$contabilitate_3821_error <- renderText({req(expunere_reactiva$all_names==FALSE)
      paste("Lipseste coloana: ",expunere_reactiva$missing_names, collapse = " ; ")      })
    
    observe({req(expunere_reactiva$all_names)
      
      expunere_reactiva$contabilitate_3821 <- expunere_reactiva$file_read_prel %>% 
        dplyr::filter(!is.na(`Partener|Cod`)) %>% dplyr::left_join(y = cod_partener_cui, by = c("Partener|Cod" = "Code")) %>%
        dplyr::rename_at(.vars = "Partener|Denumire",~"Beneficiar") %>%
          dplyr::group_by(`Partener|Cod`,CUI,Beneficiar) %>% 
            dplyr::summarise(`Sold final|Debit`=sum(`Sold final|Debit`)) %>% dplyr::ungroup() 
      
      
      output$sumar_contabilitate_3821 <- function() {
        expunere_reactiva$contabilitate_3821 %>% janitor::adorn_totals(where = "row",fill = "-",na.rm = TRUE,name = "TOTAL") %>%
          dplyr::mutate_at(.vars = "Sold final|Debit",formatC,format="f",digits=2,big.mark=",") %>%
          knitr::kable(format = "html",caption = "Upload contabilitate 3821:",align = c("c","c","c","r")) %>% 
          kableExtra::kable_styling(bootstrap_options = c("condensed"))  }
      
      isolate(expunere_agregata$final_report <- dplyr::bind_rows(expunere_agregata$final_report,
          expunere_reactiva$contabilitate_3821 %>% dplyr::select(`Cod Partener` = `Partener|Cod`,
            Expunere_Bruta = `Sold final|Debit`,CUI,Beneficiar) %>% dplyr::mutate(Tip_expunere = "comisioane_finantari")) %>%
            dplyr::mutate(Grad_risc_contrapartida = 1) %>%
            dplyr::mutate(Tipologie_expunere = "elem. De activ") %>%
            dplyr::mutate(Expunere_Neta = Expunere_Bruta * Grad_risc_contrapartida)  )
      
      isolate(expunere_agregata$progress <- expunere_agregata$progress + 5)
      
                                                                 
    })  })
  
  
  observeEvent(input$action_titluri,{
    isolate(expunere_agregata$final_report <- dplyr::bind_rows(
      if (is.null(expunere_agregata$final_report)) (expunere_agregata$final_report) else (expunere_agregata$final_report %>% 
                                                                                            dplyr::filter(Beneficiar != "Ministerul Finantelor Publice")),
      data.frame("Cod Partener" = NA_character_, Expunere_Bruta = as.numeric(input$valoare_titluri),
                 Beneficiar = "Ministerul Finantelor Publice", CUI = NA_character_,Tip_expunere = "titluri",
                 Grad_risc_contrapartida = 1,Tipologie_expunere = "elem. De activ",Expunere_Neta = 0,
                 stringsAsFactors = FALSE,check.names = FALSE) ) )
    
    isolate(expunere_agregata$progress <- expunere_agregata$progress + 5)
    
    
  })
  
  
  observeEvent(input$contabilitate_comisioane,{
    
    nume_obligatorii_contabilitate_comisioane <- c("Serie","Rest de plata","Beneficiar Garantie","Nr Contract")
    
    
    expunere_reactiva <- reactiveValues(nume_obligatorii=nume_obligatorii_contabilitate_comisioane)
    
    expunere_reactiva$file_input <- input$contabilitate_comisioane$datapath
    
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive=expunere_reactiva)
    
    
    output$sinteza_upload_comisioane <- renderTable({req(expunere_reactiva$all_names==FALSE)
      data.frame("STOP_este_posibil_sa_fi_uploadat_fisierul_gresit." = paste("Lipseste coloana: ",expunere_reactiva$missing_names))  })
    
    observe({req(expunere_reactiva$all_names)
      
      expunere_reactiva$contabilitate_comisioane <- expunere_reactiva$file_read_prel %>%
        dplyr::filter(!is.na(Serie)) %>% 
        dplyr::mutate(Tip_comision = ifelse(stringr::str_detect(Serie,"PC"),"Prima Casa",
          ifelse(stringr::str_detect(Serie,"IMM|CTG|OPT|SUN"),"Surse proprii",
      ifelse(stringr::str_detect(Serie,"PM"),"Prima masina", ifelse(stringr::str_detect(Serie,"_V"),"Excluse_expunere_agregata",
                   ifelse(stringr::str_detect(Serie,"OUG79"),"Surse_administrare","Altele"))))))
     
       isolate(expunere_agregata$final_report <- dplyr::bind_rows(expunere_agregata$final_report,
         expunere_reactiva$contabilitate_comisioane %>% dplyr::filter(Tip_comision !="Excluse_expunere_agregata")  %>% 
         dplyr::mutate(Tip_comision = ifelse(Tip_comision=="Altele","Surse proprii",Tip_comision)) %>%
          dplyr::group_by(Beneficiar = Tip_comision) %>%
          dplyr::summarise(Expunere_Bruta = sum(`Rest de plata`)) %>% dplyr::ungroup() %>%
          dplyr::mutate(Tip_expunere = "comisioane_neincasate",Grad_risc_contrapartida = 1,Expunere_Neta = Expunere_Bruta,
           Tipologie_expunere = "elem. De activ",`Cod Partener` = "Nu e cazul", CUI = "Nu e cazul") )  )  
     
       isolate(expunere_agregata$progress <- expunere_agregata$progress + 10)
       
       output$sinteza_upload_comisioane <- function() {
        data.frame("Rest_de_plata_total_uploadat"=sum(expunere_reactiva$contabilitate_comisioane$`Rest de plata`),
            "Se_exclude_suma"=sum(expunere_reactiva$contabilitate_comisioane$`Rest de plata`[
              expunere_reactiva$contabilitate_comisioane$Tip_comision == "Excluse_expunere_agregata"]),stringsAsFactors = F) %>%
          dplyr::mutate_all(formatC,format="f",digits=2,big.mark=",") %>%
          knitr::kable(format = "html",caption = "Sinteza fisier uploadat:",align = c("r","r")) %>% 
          kableExtra::kable_styling(bootstrap_options = c("condensed"))
      }
      
        output$sumar_contabilitate_comisioane <- function() {
          expunere_reactiva$contabilitate_comisioane %>% dplyr::filter(Tip_comision !="Excluse_expunere_agregata")  %>% 
            dplyr::group_by(Tip_comision) %>%
            dplyr::summarise(Expunere_Bruta = sum(`Rest de plata`)) %>%
            janitor::adorn_totals(where = "row",na.rm = TRUE,name = "TOTAL") %>%
            dplyr::mutate_at(.vars = 'Expunere_Bruta',formatC,format="f",digits=2,big.mark=",") %>%
            knitr::kable(format = "html",caption = "Voi raporta comisioanele de mai jos:",align = c("l","r")) %>% 
            kableExtra::kable_styling(bootstrap_options = c("condensed"))  }
     
        output$contabilitate_comisioane_excluse <- function() {
          expunere_reactiva$contabilitate_comisioane %>% dplyr::filter(Tip_comision =="Excluse_expunere_agregata")  %>% 
            dplyr::select(-Tip_comision) %>%
            knitr::kable(format = "html",caption = "Exclud comisioanele de mai jos din expunerea agregata:") %>% 
            kableExtra::kable_styling(bootstrap_options = c("condensed"))  }
        
        output$contabilitate_comisioane_diverse <- function() {
          expunere_reactiva$contabilitate_comisioane %>% dplyr::filter(Tip_comision == "Altele")  %>% 
            dplyr::select(5,1:4) %>% janitor::adorn_totals(where = "row",fill = "-",na.rm = TRUE,name = "Total") %>%
            dplyr::mutate_at(.vars = 'Rest de plata',formatC,format="f",digits=2,big.mark=",") %>%
            knitr::kable(format = "html",caption = "Comisioanele de mai jos le raportez ca surse proprii:",
                         align = c("l","l","r","l","l")) %>% 
            kableExtra::kable_styling(bootstrap_options = c("condensed"))  }
        
        
     
    })
    
    })
  
  
  output$sumar_expunere_agregata <- DT::renderDataTable({req(expunere_agregata$final_report)
    dt_generate_function(df = expunere_agregata$final_report %>% dplyr::group_by(Tip_expunere) %>%
                           dplyr::summarise(Nr_beneficiari=dplyr::n_distinct(`Cod Partener`),
                              Expunere_Bruta=sum(Expunere_Bruta))
                         ,round_col = 2:3,caption = "Sumar expunere agregata:")  })
  
  # Observer for generating versiune raportare report - Anexa7
  observeEvent(expunere_agregata$final_report,{
    removeUI("#expunere_agregata_ui_1-welcome_expunere_agregata")
    expunere_agregata$versiune_raportare <- tidyr::pivot_wider(data = expunere_agregata$final_report %>% 
         dplyr::group_by(`Cod Partener`,CUI,Beneficiar,Tipologie_expunere,Grad_risc_contrapartida) %>%
               dplyr::summarise(Expunere_Bruta = sum(Expunere_Bruta),Expunere_Neta=sum(Expunere_Neta)) %>% dplyr::ungroup(),
                   names_from = c(Tipologie_expunere,Grad_risc_contrapartida),
                    values_from = c(Expunere_Bruta,Expunere_Neta,Grad_risc_contrapartida),values_fill = 0,values_fn = sum)
    
    nume_expunere_agregata <- data.frame(nume_brute = c("Cod Partener", "CUI","Beneficiar","Expunere_Bruta_elem. In afara bilantului_0.5",
              "Expunere_Bruta_elem. De activ_1","Expunere_Neta_elem. In afara bilantului_0.5","Expunere_Neta_elem. De activ_1",
              "Grad_risc_contrapartida_elem. In afara bilantului_0.5","Grad_risc_contrapartida_elem. De activ_1"),
                                         nume_corecte = c("Cod Partener","CUI","Beneficiar","Expunere_Bruta_off_balance","Expunere_Bruta_activ","Expunere_neta_off_balance",
                 "Expunere_neta_activ","Grad_risc_contrapartida_off_balance","Grad_risc_contrapartida_activ"),stringsAsFactors = F)
    
    expunere_agregata$new_names <- nume_expunere_agregata$nume_corecte[match(x = names(expunere_agregata$versiune_raportare),
                                                                             table = nume_expunere_agregata$nume_brute)]
    
    })
  
  
  output$expunere_agregata_detaliata <- DT::renderDataTable({req(expunere_agregata$versiune_raportare)
    DT::datatable(data = expunere_agregata$versiune_raportare  %>% setNames(nm = expunere_agregata$new_names) %>%
         dplyr::select(dplyr::one_of(c("Cod Partener","CUI","Beneficiar","Expunere_Bruta_activ","Grad_risc_contrapartida_activ", "Expunere_neta_activ",
            "Expunere_Bruta_off_balance","Grad_risc_contrapartida_off_balance","Expunere_neta_off_balance"))) %>%
      dplyr::mutate(dplyr::across(.cols = dplyr::one_of("Grad_risc_contrapartida_activ"),
          ~ifelse(expunere_agregata$versiune_raportare$Beneficiar ==  "Ministerul Finantelor Publice",0,.x))) %>%
      dplyr::mutate("Expunere_Neta_TOTAL" = rowSums(dplyr::select(.data = .,
                  dplyr::one_of("Expunere_neta_activ","Expunere_neta_off_balance")))),extensions = 'FixedColumns',
      options = list(scrollX=TRUE, pageLength = 5,fixedColumns = list(leftColumns = 4))) %>% 
       DT::formatRound(columns = 4:(ncol(expunere_agregata$versiune_raportare)+1),digits = 2,dec.mark = ".")
    })
  
  # Expunere agregata is shown no matter where the user uploads file
  output$show_expunere_agreata <- renderUI({req(expunere_agregata$versiune_raportare)
    downloadButton(outputId = session$ns("down_expunere_agreata"),label = "Download Anexa7")})
  
  
  output$down_expunere_agreata <- downloadHandler(filename = function(){"expunere_agregata.csv"},
   content = function(file) {readr::write_csv(x = expunere_agregata$versiune_raportare  %>% setNames(nm = expunere_agregata$new_names) %>%
        dplyr::select(dplyr::one_of(c("Cod Partener","CUI","Beneficiar","Expunere_Bruta_activ","Grad_risc_contrapartida_activ", "Expunere_neta_activ",
        "Expunere_Bruta_off_balance","Grad_risc_contrapartida_off_balance","Expunere_neta_off_balance"))) %>%
        dplyr::mutate("Expunere_Neta_TOTAL" = rowSums(dplyr::select(.data = .,
        dplyr::one_of("Expunere_neta_activ","Expunere_neta_off_balance")))),path = file)})
  
 
  output$show_button_cui <- renderUI({req(expunere_agregata$versiune_raportare)
    fluidRow(
    column(width=6,actionButton(inputId = session$ns("action_cui"),
      label = "Click aici pentru a cauta CUI-urile lipsa in lista de additional data ",
      icon = icon("search"))),column(width=6, div(class = "pull-right",
            actionButton(inputId = session$ns("toggle_cui_data"),icon=icon("toggle"),
                label = "Hide/Show CUI additional data. Aici voi cauta CUI-urile lipsa." )))) })
  
  # Observer to show CUI additional data. The observer contains output of cui additional data because this output starts hidden.
  observeEvent(input$toggle_cui_data,{
    shinyjs::toggle("additional_cui_data") 
    output$additional_cui_data <- DT::renderDataTable({req(expunere_agregata$versiune_raportare)
      
      dt_generate_function(df = additional_cui_match,
                           caption = "Voi actualiza CUI-urile lipsa folosind lista de mai jos:") })
    })
    
  
  observeEvent(expunere_agregata$versiune_raportare,{
    expunere_agregata$cui_lipsa <- expunere_agregata$versiune_raportare %>% 
      dplyr::filter(is.na(CUI) | CUI == "<nespec>") %>%
        dplyr::select("Cod Partener","CUI","Beneficiar")
    
    output$expunere_agregata_CUI_lipsa <- DT::renderDataTable({
      dt_generate_function(df = expunere_agregata$cui_lipsa,caption = "Lista CUI-urilor lipsa:")  })
    })
  
  observeEvent(input$action_cui,{
    #removeUI("#expunere_agregata_ui_1-action_cui")
    expunere_agregata$versiune_raportare <- expunere_agregata$versiune_raportare %>% 
              dplyr::mutate(dplyr::across(.cols = CUI, ~ifelse(is.na(.x) | .x == "<nespec>",
                 additional_cui_match$CUI[match(expunere_agregata$versiune_raportare$Beneficiar,
                                          table = additional_cui_match$Beneficiar)],.x)))  
    })
  
  output$expunere_agregata_nume_dublat <- DT::renderDataTable({req(expunere_agregata$versiune_raportare)
    DT::datatable(data = expunere_agregata$versiune_raportare %>% 
                    dplyr::filter(Beneficiar %in% expunere_agregata$versiune_raportare$Beneficiar[duplicated(expunere_agregata$versiune_raportare$Beneficiar)]) %>% 
                    dplyr::bind_rows(dplyr::filter(.data = expunere_agregata$versiune_raportare, is.na(Beneficiar))) %>%
                    dplyr::select("Cod Partener","CUI","Beneficiar"), caption = "Nume beneficiari dublati sau lipsa:",
                  options = list(info=FALSE,paging=FALSE,searching=FALSE)) })
  
  output$expunere_agregata_cod_partener_dublat <- DT::renderDataTable({req(expunere_agregata$versiune_raportare)
    DT::datatable(data = expunere_agregata$versiune_raportare %>% 
                    dplyr::filter(`Cod Partener` %in% expunere_agregata$versiune_raportare$`Cod Partener`[duplicated(expunere_agregata$versiune_raportare$`Cod Partener`)]) %>% 
                    dplyr::bind_rows(dplyr::filter(.data = expunere_agregata$versiune_raportare, is.na(`Cod Partener`))) %>%
                    dplyr::select("Cod Partener","CUI","Beneficiar"), caption = "Cod partener dublat sau lipsa:",
                  options = list(info=FALSE,paging=FALSE,searching=FALSE)) })
   
   
  
}
     
     
  
## To be copied in the UI
# mod_expunere_agregata_ui("expunere_agregata_ui_1")
    
## To be copied in the server
# callModule(mod_expunere_agregata_server, "expunere_agregata_ui_1")
 
