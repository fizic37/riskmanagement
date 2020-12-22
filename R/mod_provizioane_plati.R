#' provizioane_plati UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_provizioane_plati_ui <- function(id){
  ns <- NS(id)
  
  tagList(shinydashboard::box(title = "Sinteza provizioane plati",width = 12,DT::dataTableOutput(ns("sumar_raport")), br(),
                downloadButton(outputId = ns("down_raport_plati"),label = "Download raportul selectat detaliat"),
                collapsible = TRUE,collapsed = T,status = "success"),
          shinydashboard::box(title = "Data raport plati",width = 12,collapsible = FALSE,status = "danger",
                              selectInput(inputId = ns("data_raport_plati"),width = "300px",
                      label = "Selecteaza de aici data raportului",choices = as.Date("2019-12-31"))),
         
          shinydashboard::box(title = "Sensibilitate provizioane plati",status = "success",collapsible = T,collapsed = T,width = 12,
        column(width = 12, fillRow(flex = c(1, NA), numericInput(inputId = ns("coef_garantii_plati"),  value = 0.25,width = '500px',
                    label = "Coeficient ajustare garantii accesorii-simulare" ),
                        numericInput(inputId = ns("coef_ctg_plati"),value = 0, width='500px',
                           label = "Coeficient ajustare expunere din contragarantii-simulare")),
               hr(),  DT::dataTableOutput(ns("sumar_simulare")))),
       
    shinydashboard::box(title = "Grad anual de recuperare creante nete",status = "success",width = 12,collapsible = T,collapsed = T,
        column(width = 12,DT::dataTableOutput(ns("sumar_creante"))),
        column(width=12,br()),
        column(width = 6,downloadButton(outputId = ns("down_sumar_creante"),label = "Download detaliat creante recuperate"))),
        
    shinydashboard::box(title = "Grad recuperare expunere CTG",status = "success",width = 12,collapsible = T,collapsed = F,
        column(width = 12,DT::dataTableOutput(ns("sumar_recuperari_ctg"))),
        column(width=12,br()),
        #verbatimTextOutput(ns("diverse")),
        column(width=12,downloadButton(outputId = ns("down_recuperare_ctg"),label = "Download detaliat"))))
        
  
  
}
    
#' provizioane_plati Server Function
#'
#' @noRd 
mod_provizioane_plati_server <- function(input, output, session){
  ns <- session$ns
  load('data/baza_provizioane_plati.rda')
  
  sumar_raport_database <- baza_provizioane_plati %>% dplyr::group_by(data_raport) %>% 
    dplyr::summarise(Nr_contracte = dplyr::n(), PlatiBrute = sum(PlatiEfective), RecuperariTotale = sum(TotalRecuperat),
        Garantii_accesorii_admise =  sum(ValoareAdmisaFNG), Expunere_CTG_plati = sum(Expunere_CTG_plati),  
        Provizioane_Constituite = sum(ProvizionNou), CreanteNete = PlatiBrute - RecuperariTotale,  
        Grad_Acoperire_Provizioane =  Provizioane_Constituite / CreanteNete)
  
  output$sumar_raport <- DT::renderDataTable({dt_generate_function(df = sumar_raport_database,round_col = 2:8,perc_col = 9,
                            caption = "Sumar baza date Provizioane Plati:")})
  
  output$down_raport_plati <- downloadHandler(filename = function(){"provizioane_plati.csv"},content = function(file){
    readr::write_csv(x = baza_provizioane_plati, path=file) })
  
  updateSelectInput(session = session,inputId = "data_raport_plati",choices = unique(baza_provizioane_plati$data_raport),
                 label =  "Selecteaza de aici data raportului" )
  
  
  baza_selectata <- reactive({ baza_provizioane_plati %>% dplyr::filter(data_raport == as.Date(input$data_raport_plati)) }) #names of baza splitata are unique values of data_raport
  
  
  baza_selectata_simulata <- reactive({ baza_selectata() %>% dplyr::mutate(Provizion_Simulat = pmax(PlatiEfective - TotalRecuperat - 
                                              input$coef_garantii_plati*ValoareAdmisaFNG - input$coef_ctg_plati*Expunere_CTG_plati,0)) })
  
  
  
  sumar_simulare <- reactive({ baza_selectata_simulata() %>% dplyr::summarise(Creante_Nete = sum(PlatiEfective) - sum(TotalRecuperat), 
                    ProvizionContabil = sum(ProvizionNou),Provizion_Simulat = sum(Provizion_Simulat),
                    Grad_acoperire_provizioane_simulat = Provizion_Simulat/Creante_Nete) %>%
      dplyr::mutate(Impact_Provizion_Simulat = .[[3]] - .[[2]]) %>% dplyr::select(1:3,5,4) %>% 
      dplyr::rename_at(.vars = 1:5,~paste0(.,"_",input$data_raport_plati)) })
  
  output$sumar_simulare <- DT::renderDataTable({
    dt_generate_function(df=sumar_simulare(),caption = "Provizioane rezultate in urma 
        simularii. Selectati valorile de mai sus si se va actualiza valoarea corespunzatoare a provizionului simulat mai jos:",
                         round_col = 1:4,perc_col = 5) })
  
  baza_splitata <- dplyr::group_split(.tbl = baza_provizioane_plati,data_raport) %>% purrr::map(.f = ~dplyr::as_tibble(x=.x))
  
  for (i in 1:length(baza_splitata)) {
    baza_splitata[[i]] <- dplyr::rename_at(.tbl = baza_splitata[[i]],.vars = "Plata_neta",
                                           .funs = list(~paste0(.,'_',baza_splitata[[i]]$data_raport[1])))
  }
  
  names(baza_splitata) <- purrr::map_dbl(baza_splitata,~unique(.x$data_raport)) %>% as.Date.numeric(origin = "1970-01-01")
  
  #data_raport <- as.Date("2017-12-31")
  
  baza_selectata <- reactive({ baza_splitata[which(names(baza_splitata) == input$data_raport_plati)][[1]]  })
  
  baza_splitata_filtrata <- reactive({ baza_splitata[which(names(baza_splitata) > input$data_raport_plati) ] %>% 
    purrr::map(~dplyr::select(.data = .x,"DocumentId",dplyr::matches("Plata_neta"))) })
  
  baza_plati_nete_brut <- reactive({ 
    
    purrr::map(.x = baza_splitata_filtrata(),~dplyr::left_join(x = dplyr::select(baza_selectata(),
                                      DocumentId,dplyr::matches("Plata_neta")),y =.x, by="DocumentId")) })
  
  baza_plati_nete <- reactive({ 
    shiny::validate(shiny::need(length(baza_plati_nete_brut())>0,message = "Nu pot calcula grad de recuperare pentru cea mai recenta data selectata"))
    cbind(dplyr::select(.data = baza_selectata(),"DocumentId",dplyr::matches("Plata_neta")), # preiau 2 coloane, DocumentId si creantele brute la data de referinta
          purrr::map_dfc(.x = baza_plati_nete_brut(),.f = ~cbind(dplyr::select(.x,-1,-2)))) %>%  # adaug restul creantelor dupa data de referinta
        dplyr::mutate_all(~tidyr::replace_na(data = .,replace = 0)) })  # inlocuiesc valorile NA cu zero - creanta recuperata in totalitate
  
  output_recuperari_plati <- reactive({ 
        baza_plati_nete() %>% dplyr::summarise_all(.funs = ~sum(.)) %>% dplyr::select(-DocumentId) %>% 
    tidyr::pivot_longer(cols = dplyr::matches("Plata_neta")) %>% 
    setNames(nm=c("Indicator","Valoare_creanta_neta")) %>% 
    dplyr::mutate(Recuperari_brute=-1*(Valoare_creanta_neta-Valoare_creanta_neta[1])) %>%
    dplyr::mutate(Grad_recuperare_cumulat=Recuperari_brute/Valoare_creanta_neta[1])  })
  
  caption_baza_plati_nete <- reactive({ paste("Creantele nete la data de ",as.character(input$data_raport_plati),"in valoare de ",
                                   formatC(sum(baza_plati_nete()[,2]),digits = 0,big.mark = ",",format="f"),
                                   " lei au fost recuperate intr-un procent cumulat de ",
                                   round(output_recuperari_plati()[nrow(output_recuperari_plati()),4]*100,1),"%")  })
  
  output$sumar_creante <- DT::renderDataTable({
    
    dt_generate_function(df = output_recuperari_plati(),round_col = 2:3,perc_col = 4,
                         caption = caption_baza_plati_nete())  })
  
  output$down_sumar_creante <- downloadHandler(filename = function(){"creante_recuperate.csv"},content = function(file){
    readr::write_csv(x = baza_plati_nete(),path = file) } )
  
  # FRC grad de recuperare
  
  frc <- readxl::read_excel("inst/extdata/incasari_frc.xlsx",col_types = c("numeric","text","date","numeric"))
  frc$`Data incasarii` <- as.Date(frc$`Data incasarii`)
  
  baza_selectata_frc <- reactive({ dplyr::left_join(baza_selectata(),y = frc[,-2],by="DocumentId") %>% 
              tidyr::replace_na(list(`Suma incasata FRC (RON)`=0)) })
  
  sumar_recuperari_ctg <- reactive({ baza_selectata_frc() %>% dplyr::filter(Expunere_CTG_plati>0,!is.na(`Data incasarii`)) %>% 
      dplyr::group_by(Anul_incasarii_FRC=lubridate::year(`Data incasarii`)) %>% 
      dplyr::summarise(Suma_incasata_FRC=sum(`Suma incasata FRC (RON)`)) %>% 
      dplyr::mutate(Grad_anual_recuperare=Suma_incasata_FRC/sum(baza_selectata()$Expunere_CTG_plati)) %>% 
      dplyr::mutate(Grad_cumulat_recuperare=cumsum(Grad_anual_recuperare))  })
  
  caption_recuperari_frc <- reactive({paste("Valoarea expunerii CTG din plati","la data de ",as.character(input$data_raport_plati),
                " in valoare de ", formatC(sum(baza_selectata()$Expunere_CTG_plati,na.rm = T), format="f",digits = 0,big.mark = ","),
                " a fost recuperata dupa cum se prezinta in tabelul de mai jos. Ultima data de actualizare a incasarilor FRC este  ",
                                            as.character(max(frc$`Data incasarii`,na.rm = TRUE))) })
  output$sumar_recuperari_ctg <- DT::renderDataTable({
    dt_generate_function(df = sumar_recuperari_ctg(),round_col = 2,perc_col = 3:4,
                         caption = caption_recuperari_frc())  })
  
  output$down_recuperare_ctg <-  downloadHandler(filename = function(){"recuperare_ctg.csv"},content = function(file){
    readr::write_csv(x = baza_selectata_frc() %>% dplyr::filter(Expunere_CTG_plati>0,!is.na(`Data incasarii`)),path = file) } )
  
  #output$diverse <- renderPrint({c(str(frc),str(baza_selectata()))})
  
}
    
## To be copied in the UI
# mod_provizioane_plati_ui("provizioane_plati_ui_1")
    
## To be copied in the server
# callModule(mod_provizioane_plati_server, "provizioane_plati_ui_1")
 
