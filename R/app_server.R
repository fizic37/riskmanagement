#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  login <- FALSE
  sidebar_selected <- c()
  #nume_obligatorii_excel <- c()
  #excel_input <- NULL
  #vals <- reactiveValues(nume_obligatorii_excel=nume_obligatorii_excel,excel_input=excel_input,
           #              login=login,sidebar_selected = sidebar_selected)
 # portofoliu_database <- data.frame()
 
  vals <- reactiveValues(login=login,sidebar_selected = sidebar_selected)
  
  #ATTENTION: calling modules with vals means the module may be called again every time corresponding vals changes, especially if
  # you want to balance vals between modules (from one module to another and viceversa)
  
  callModule(mod_loginpage_server, "loginpage_ui_1",vals)
  callModule(mod_sidebar_server, "sidebar_ui_1",vals)
  #callModule(mod_home_server, "home_ui_1",vals)
  
  # Use if, there is no else if. Daca as folosi else if inseamna ca doar daca nu ai selectat niciodata home call celelalte module
 observeEvent(vals$sidebar_selected,{
   
    if (sum("home" == vals$sidebar_selected)==1) {
      callModule(mod_home_server, "home_ui_1",vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"home")
      }
    
    if (sum("solduri" == vals$sidebar_selected)==1) {
      callModule(mod_portofoliu_server, "portofoliu_ui_1")
      vals$sidebar_selected <- c(vals$sidebar_selected,"solduri")
      }  # this speeds up the process. vals$sidebar_selected se updateaza greu.
                                                                    # user-ul schimba mult mai repede selectia sidebar decat se updateaza vals$sidebar_selected
    if (sum("database-portofoliu" == vals$sidebar_selected)==1) {
      callModule(mod_database_portofoliu_server, "database_portofoliu_ui_1",vals)
      callModule(mod_database_portofoliu_upload_server, "database_portofoliu_upload_ui_1")
      callModule(mod_database_util_files_server, "database_util_files_ui_1")   
      vals$sidebar_selected <- c(vals$sidebar_selected,"database-portofoliu")}
    
    if (sum("database_CRC"==vals$sidebar_selected )==1)  {
      callModule(mod_crc_server, "crc_ui_1") 
      vals$sidebar_selected <- c(vals$sidebar_selected,"database_CRC")
    }
    
   if (sum("database_CIP" == vals$sidebar_selected)==1) {
     callModule(mod_cip_server, "cip_ui_1")
     vals$sidebar_selected <- c(vals$sidebar_selected,"database_CIP")}
   
   if (sum("plati" == vals$sidebar_selected)==1) {
     callModule(mod_provizioane_plati_server, "provizioane_plati_ui_1")
     vals$sidebar_selected <- c(vals$sidebar_selected,"plati")}
   
   if (sum("database_plati" == vals$sidebar_selected)==1) {
     callModule(mod_database_upload_plati_server, "database_upload_plati_ui_1")
     vals$sidebar_selected <- c(vals$sidebar_selected,"database_plati")}
   
   if (sum("grupuri" == vals$sidebar_selected)==1) {
     callModule(mod_database_grupuri_server, "database_grupuri_ui_1")
     vals$sidebar_selected <- c(vals$sidebar_selected,"grupuri")  }
   
   if (sum("expunere_agregata" == vals$sidebar_selected)==1) {
     callModule(mod_expunere_agregata_server, "expunere_agregata_ui_1")
     vals$sidebar_selected <- c(vals$sidebar_selected,"expunere_agregata")  }
   
   if (sum("garantii_colaterale" == vals$sidebar_selected)==1) {
     callModule(mod_garantii_colaterale_server, "garantii_colaterale_ui_1")
     vals$sidebar_selected <- c(vals$sidebar_selected,"garantii_colaterale")  }
   
   if (sum("raportare_bnr" == vals$sidebar_selected)==1) {
     callModule(mod_raportare_bnr_server, "raportare_bnr_ui_1")
     vals$sidebar_selected <- c(vals$sidebar_selected,"raportare_bnr")  }
   
   if (sum("ifrs_portofoliu" == vals$sidebar_selected)==1) {
     callModule(mod_ifrs_portofoliu_server, "ifrs_portofoliu_ui_1")
     vals$sidebar_selected <- c(vals$sidebar_selected,"ifrs_portofoliu")  }
 })
  
}
