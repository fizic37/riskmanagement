#' ifrs_portofoliu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ifrs_portofoliu_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabBox(title="",width=NULL,
         tabPanel(title = "Calcul Provizioane IFRS9 portofoliu"),
         tabPanel(title = "Dezvoltare Modele IFRS9",
                  fluidRow(shinydashboard::box(title = "Model 1Y",status = "success",width = 6,
                  footer = "Model 1Y genereaza probabilitatile de default la un an. 
                  Au fost generate 2 modele, unul pentru beneficiarii raportati in CRC
                    si unul pentru restul, avand in vedere ca informatiile din CRC au o pondere semnificativa
                                              in estimarea riscului de default",
                   shinydashboardPlus::accordion(inputId = ns("accordion_1Y"),
                     shinydashboardPlus::accordionItem(
                                                       title = "Training and test data dimensions",
                                                       color = "success",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("sumar_dezvoltare_model_1Y"))),
                                         shinydashboardPlus::accordionItem(
                                                       title = "AUC for the 2 models",
                                                       color = "success",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("sumar_rezultate_model_1Y"))),
                                         shinydashboardPlus::accordionItem(
                                                       title = "Model coefficients with CRC",
                                                       color = "success",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("coeficienti_model_1Y_with_CRC"))),
                                         shinydashboardPlus::accordionItem(
                                                       title = "Model coefficients NO CRC",
                                                       color = "success",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("coeficienti_model_1Y_no_crc"))),
                                         shinydashboardPlus::accordionItem(
                                                       title = "Mean encode 1Y",
                                                       color = "success",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("mean_encode_1Y"))))),
                shinydashboard::box(title = "Model 3Y",status = "success",width = 6,collapsible = F,
                footer = "Model 3Y genereaza probabilitatile de default la 3 ani. Au fost generate 2 modele, 
                      in functie de raportarea beneficiarilor in cadrul CRC",
                    shinydashboardPlus::accordion(inputId = ns("accordion_3Y"),
                      shinydashboardPlus::accordionItem(
                                                       title = "Training and test data dimensions",
                                                       color = "success",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("sumar_dezvoltare_model_3Y"))),
                       shinydashboardPlus::accordionItem(
                                                       title = "AUC for the 2 models",
                                                       color = "success",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("sumar_rezultate_model_3Y"))),
                                         shinydashboardPlus::accordionItem(
                                                       title = "Model coefficients with CRC",
                                                       color = "success",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("coeficienti_model_3Y_with_CRC"))),
                                         shinydashboardPlus::accordionItem(
                                                       title = "Model coefficients NO CRC",
                                                       color = "success",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("coeficienti_model_3Y_no_crc"))),
                                         shinydashboardPlus::accordionItem(
                                                       title = "Mean encode 3Y",
                                                       color = "success",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("mean_encode_3Y"))))
                           ))
                  ,shinydashboardPlus::accordion(inputId = ns("accordion_graphs"),
                    shinydashboardPlus::accordionItem(color = "success",collapsed = TRUE,
                                      title = "Evolutia ratei efective de default la un an si a probabilitatii
                                        medii de default la un an prognozate de model",
                                           plotOutput(outputId = ns('graph1'))),
                    shinydashboardPlus::accordionItem(color = "success",collapsed = TRUE,
                                      title = "Evolutia coeficientului fwd looking precum si a valorii 
                                          prognozate de ecuatia de regresie",
                                           plotOutput(outputId = ns('graph2'))))
         ))
         
}
    
#' ifrs_portofoliu Server Function
#'
#' @noRd 
mod_ifrs_portofoliu_server <- function(input, output, session){
  ns <- session$ns
  
  load("inst/extdata/ifrs9/modelling_results.rda")
  
  df_fwd_coef <- readr::read_csv("inst/extdata/ifrs9/fwd_coef.csv")
  
  
  output$sumar_dezvoltare_model_1Y <- DT::renderDataTable({ dt_generate_function(round_col = 2,
            df = modelling_results %>% dplyr::filter(stringr::str_detect(indicator,pattern = "nr_observatii")) %>% 
              dplyr::filter(stringr::str_detect(indicator,pattern = "1Y")))})
 
  output$sumar_rezultate_model_1Y <- DT::renderDataTable({
    
      dt_generate_function(digits = 2,  round_col = 2,
        df = modelling_results %>% dplyr::filter(stringr::str_detect(indicator, pattern = "auc")) %>% 
          dplyr::filter(stringr::str_detect(indicator, pattern = "1Y"))
      )
    })
  
  output$coeficienti_model_1Y_with_CRC <- DT::renderDataTable({
    model_1Y_with_CRC <- readRDS(file = "inst/extdata/ifrs9/model_1Y_with_CRC")
      
      dt_generate_function(round_col = 2,  digits = 6,
        df = predict(object = model_1Y_with_CRC, type = "coefficients",
            s = model_1Y_with_CRC$lambda.min) %>% broom::tidy() %>% dplyr::select(-2)
      )
    })
  
  output$coeficienti_model_1Y_no_crc <- DT::renderDataTable({
    model_1Y_no_crc <- readRDS(file = "inst/extdata/ifrs9/model_1Y_no_crc")
    
      dt_generate_function(round_col = 2,  digits = 6,
        df = predict(object = model_1Y_no_crc, type = "coefficients",
            s = model_1Y_no_crc$lambda.min) %>% broom::tidy() %>% dplyr::select(-2) )    })
  
  output$mean_encode_1Y <- DT::renderDataTable({
    categ_contam_1Y <- readRDS(file = "inst/extdata/ifrs9/categ_contaminata_1Y")
    
      dt_generate_function(round_col = 1:3,   digits = 6,
        caption = "Valorile aferente variabilei mean_encode_categ_contaminata_1Y in functie de categoria 
              contaminata a beneficiarului",
        df = categ_contam_1Y$crossFrame %>% dplyr::select(1, 3:5) %>% dplyr::mutate(
          insolventa = categorie_contaminata_lev_x_insolventa * categorie_contaminata_catP,
          instiintare_neplata = categorie_contaminata_lev_x_instiintare_neplata * categorie_contaminata_catP,
          standard = categorie_contaminata_lev_x_standard * categorie_contaminata_catP) %>%
          dplyr::select(insolventa, instiintare_neplata, standard) %>% 
            dplyr::summarise_all( ~max(.))   )   })
  
  output$sumar_dezvoltare_model_3Y <- DT::renderDataTable({
      dt_generate_function(round_col = 2,
        df = modelling_results %>% dplyr::filter(stringr::str_detect(indicator, pattern = "nr_observatii")) %>% 
          dplyr::filter(stringr::str_detect(indicator, pattern = "3Y"))
      )
    })
  
  output$sumar_rezultate_model_3Y <- DT::renderDataTable({
      dt_generate_function(digits = 2, round_col = 2,
        df = modelling_results %>% dplyr::filter(stringr::str_detect(indicator, pattern = "auc")) %>% 
            dplyr::filter(stringr::str_detect(indicator, pattern = "3Y"))
      )
    })
  
  output$coeficienti_model_3Y_with_CRC <- DT::renderDataTable({
    model_3Y_with_CRC <- readRDS(file = "inst/extdata/ifrs9/model_3Y_with_CRC")
    
      dt_generate_function(round_col = 2,  digits = 6,
        df = predict(object = model_3Y_with_CRC, type = "coefficients",
            s = model_3Y_with_CRC$lambda.min) %>% broom::tidy() %>% dplyr::select(-2) )   })
  
  output$coeficienti_model_3Y_no_crc <- DT::renderDataTable({
    model_3Y_no_crc <- readRDS(file = "inst/extdata/ifrs9/model_3Y_no_crc")
    
      dt_generate_function(round_col = 2,  digits = 6,
        df = predict(object = model_3Y_no_crc,type = "coefficients",
            s = model_3Y_no_crc$lambda.min) %>% broom::tidy() %>% dplyr::select(-2)    )  })
  
  output$mean_encode_3Y <- DT::renderDataTable({
    categ_contam_3Y <- readRDS(file = "inst/extdata/ifrs9/categorie_contaminata_3Y")
    
      dt_generate_function(round_col = 1:3,  digits = 6,
        caption = "Valorile aferente variabilei mean_encode_categ_contaminata_3Y in functie de 
        categoria contaminata a beneficiarului",
        df = categ_contam_3Y$crossFrame %>% dplyr::select(1, 3:5) %>% dplyr::mutate(
          insolventa = categorie_contaminata_lev_x_insolventa * categorie_contaminata_catP,
          instiintare_neplata = categorie_contaminata_lev_x_instiintare_neplata * categorie_contaminata_catP,
          standard = categorie_contaminata_lev_x_standard * categorie_contaminata_catP) %>%
            dplyr::select(insolventa, instiintare_neplata, standard) %>% dplyr::summarise_all(~  max(.))  )  })
  
  output$graph1 <- renderPlot({
    
    
    ggplot2::ggplot(data = tidyr::pivot_longer(data = 
                df_fwd_coef[-c(1,nrow(df_fwd_coef)),],cols = 2:3),
        mapping = ggplot2::aes(x = lubridate::year(anul_de_raportare), y=value, colour=name))+ ggplot2::geom_line(size=1.2)+
      ggplot2::scale_color_manual(values = c("#f3d112","#00a65a"))+
      ggplot2::scale_x_continuous(breaks = seq(2010,2018,by=1)) +  
      ggplot2::scale_y_continuous(labels = scales::percent,limits = c(0.02,NA))+
      ggplot2::ylab(label = "") + ggplot2::xlab(label = "Final de an") + 
      ggplot2::labs(title = "Evolutia ratei efective de default la un an si a probabilitatii medii de default 
                     la un an prognozate de model") + 
      ggplot2::geom_text(mapping = ggplot2::aes(y = value,label=paste0(round(value*100,1),"%"))) + 
      ggplot2::theme_set(ggplot2::theme_grey(base_size = 16))+
      ggplot2::theme(panel.grid = ggplot2::element_blank(),panel.background = ggplot2::element_rect(fill = "white"),
            legend.position = c(0.1,0.2),legend.title = ggplot2::element_blank()) })
  
  
  output$graph2 <- renderPlot({ 
    
    ggplot2::ggplot(data = tidyr::pivot_longer(data = df_fwd_coef[-1,],cols = 5:6),
            mapping = ggplot2::aes(x = lubridate::year(anul_de_raportare),y=value,colour=name))+ 
      ggplot2::geom_line(size=1.2)+
      ggplot2::scale_color_manual(values = c("#f3d112","#00a65a"))+
      ggplot2::scale_x_continuous(breaks = seq(2010,2019,by=1)) +  
      ggplot2::ylab(label = "") + ggplot2::xlab(label = "Final de an") + 
      ggplot2::labs(#caption = "In tab-ul urmator sunt prezentate detaliile ecuatiei de regresie care sta la baza estimarii coeficientului fwd looking (predict_coef)",
        title = "Evolutia coeficientului fwd looking precum si a valorii prognozate de ecuatia de regresie") + 
      ggplot2::geom_text(mapping = ggplot2::aes(y = value,label=round(value,2)),check_overlap = TRUE) + 
      ggplot2::theme_set(ggplot2::theme_grey(base_size = 16))+
      ggplot2::theme(panel.grid = ggplot2::element_blank(),panel.background = ggplot2::element_rect(fill = "white"),
            legend.position = c(0.1,0.8),legend.title = ggplot2::element_blank()) })  
  
  
  
  
}
    
## To be copied in the UI
# mod_ifrs_portofoliu_ui("ifrs_portofoliu_ui_1")
    
## To be copied in the server
# callModule(mod_ifrs_portofoliu_server, "ifrs_portofoliu_ui_1")
 
