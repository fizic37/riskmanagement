#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  
 shinydashboard::sidebarMenuOutput(outputId = ns("sidebar"))

 }
    
#' sidebar Server Function
#'
#' @noRd 
mod_sidebar_server <- function(input, output, session,vals){
  ns <- session$ns
 
  risk_user_sidebar <- shinydashboard::sidebarMenu(id = ns("tabs"),
                            shinydashboard::menuItem(tabName = "home",text = "Home",icon = icon("home")),
                            shinydashboard::menuItem(tabName = "plati",  text = "Provizioane plati",icon=icon("euro-sign"),selected = FALSE),
                            shinydashboard::menuItem(tabName = "solduri", text = "Portofoliu",icon = icon("book")),
                            shinydashboard::menuItem(text = "Database files stored",icon = icon("database"),
                                                                            tabName = "database1",selected = FALSE,
                                          shinydashboard::menuSubItem(text = "Portofoliu",tabName = "database-portofoliu",selected = FALSE),
                                          shinydashboard::menuSubItem(text = "CRC",tabName = "database_CRC",selected = FALSE)
                                          ,shinydashboard::menuSubItem(text = "CIP",tabName = "database_CIP",selected = FALSE),
                                          shinydashboard::menuSubItem(text = "Plati",tabName = "database_plati",selected = TRUE),
                                          shinydashboard::menuSubItem(text = "Grupuri",tabName = "grupuri",icon = icon("users"),selected = FALSE)),
                          shinydashboard::menuItem(tabName = "expunere_agregata", text = "Expunerea Agregata",icon = icon("pound-sign"),
                                                   selected = FALSE),
                          #,shinydashboard::menuItem(tabName = "garantii_colaterale", text = "Garantii Colaterale",icon = icon("landmark"),selected = FALSE)
                          shinydashboard::menuItem(tabName = "raportare_bnr", text = "Raportare BNR Prima Casa",icon = icon("landmark"),
                                                   selected = FALSE)  )
  
  guest_user_sidebar <- shinydashboard::sidebarMenu(id = ns("tabs"),
                          shinydashboard::menuItem(tabName = "home",text = "Home"),
                          shinydashboard::menuItem(tabName = "plati",  text = "Provizioane plati",selected = FALSE),
                          shinydashboard::menuItem(tabName = "portofoliu", text = "Portofoliu",selected = FALSE))
  
  
  
  output$sidebar <- shinydashboard::renderMenu({
    
    if (vals$login==FALSE) {return()}
    else if (vals$login==TRUE && vals$user_type=="guest-user") {return(guest_user_sidebar)}
    else {return(risk_user_sidebar)}
    
    
  })
  
  observeEvent(input$tabs,{ 
        # I use this in order to have a selection of all inputs in sidebar. This way, I don`t have to call modules
    # every time a sidebar is selected, I only call modules ones.`
    vals$sidebar_selected <- c(vals$sidebar_selected,input$tabs)
    #vals$sidebar_selected <- input$tabs
    })
  
 
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# callModule(mod_sidebar_server, "sidebar_ui_1")
 
