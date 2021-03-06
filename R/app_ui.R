#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    #gfonts::use_font(id = "playfair-display",css_path = 'inst/app/www/playfair_regular.css',selector = "logo"),
    shinydashboard::dashboardPage(skin="yellow",fresh::use_theme("inst/app/www/custom.css"),
      
      
      header = shinydashboard::dashboardHeader(title = "FNGCIMM"),
    
                         
      sidebar = shinydashboard::dashboardSidebar(mod_sidebar_ui("sidebar_ui_1"),collapsed = FALSE),
                                  
      body = shinydashboard::dashboardBody(
        gfonts::use_font(id = "pt-sans",css_path = 'inst/app/www/pt_sans_regular.css'),
                              mod_loginpage_ui("loginpage_ui_1"),
                            shinydashboard::tabItems(
                              shinydashboard::tabItem(tabName = "solduri", mod_portofoliu_ui("portofoliu_ui_1"))
                              ,shinydashboard::tabItem(tabName = "home",mod_home_ui("home_ui_1")),
                        shinydashboard::tabItem(tabName = "plati",mod_provizioane_plati_ui("provizioane_plati_ui_1")),
                              shinydashboard::tabItem(tabName = "database-portofoliu", 
                                tagList(mod_database_portofoliu_upload_ui("database_portofoliu_upload_ui_1"),
                                  mod_database_portofoliu_ui("database_portofoliu_ui_1"),
                                        mod_database_util_files_ui("database_util_files_ui_1"))),
                              shinydashboard::tabItem(tabName = "database_CRC", mod_crc_ui("crc_ui_1")),
                              shinydashboard::tabItem(tabName = "database_CIP", mod_cip_ui("cip_ui_1")),
                        shinydashboard::tabItem(tabName = "database_plati", mod_database_upload_plati_ui("database_upload_plati_ui_1")),
                        shinydashboard::tabItem(tabName = "grupuri", mod_database_grupuri_ui("database_grupuri_ui_1")),
                        shinydashboard::tabItem(tabName = "expunere_agregata", mod_expunere_agregata_ui("expunere_agregata_ui_1")),
                        shinydashboard::tabItem(tabName = "garantii_colaterale", mod_garantii_colaterale_ui("garantii_colaterale_ui_1")),
                        shinydashboard::tabItem(tabName = "raportare_bnr",mod_raportare_bnr_ui("raportare_bnr_ui_1")),
                        shinydashboard::tabItem(tabName = "ifrs_portofoliu", mod_ifrs_portofoliu_ui("ifrs_portofoliu_ui_1"))
                              )))
    
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'RiskManagementGolem'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

