#' read_excel_colaterale UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_read_excel_colaterale_ui <- function(id){
  ns <- NS(id)
}
    
#' read_excel_colaterale Server Function
#'
#' @noRd 
mod_read_excel_colaterale_server <- function(input, output, session,excel_reactive){
  ns <- session$ns
 
  observeEvent(excel_reactive$file_input,{
    
    sheets_read <- eventReactive(excel_reactive$file_input,{
      shiny::validate(shiny::need(tools::file_ext(excel_reactive$file_input) %in% c("xlsx","xls"), message = FALSE))
      readxl::excel_sheets(excel_reactive$file_input) })
    
    if (length(sheets_read())>1) {
      
      shinyWidgets::inputSweetAlert(session = session,inputId = session$ns("ok_sheet"), input = "select",
                                    inputOptions = sheets_read(),type = "warning",
                                    title = "STOP, fiserul ure mai multe sheet-uri", "Selecteaza de mai jos sheet-ul pe care sa-l citesc")
      
      selected_sheet <- eventReactive(input$ok_sheet,{input$ok_sheet})
    }
    
    else if (length(sheets_read())==1) {
      selected_sheet <- reactive({1})
    }
    
    # First read of the excel
    
    excel_first_read <- reactive({req(selected_sheet())
      readxl::read_excel(excel_reactive$file_input,sheet = selected_sheet(), range = "A1:AA50")  })
    
    
    # I get the row index where name if the columns are
    index_citire <- reactive({ req(excel_first_read())
      apply(excel_first_read(),1,function(x) (sum(excel_reactive$nume_obligatorii %in% x)>=2)) %>% 
        which(TRUE) %>% max(0,na.rm = TRUE)})
    
    
    observe({
      
      excel_reactive$file_read <- readxl::read_excel(excel_reactive$file_input,sheet = selected_sheet(), skip = index_citire()) 
        
      excel_reactive$all_names <- excel_reactive$nume_obligatorii %in% names(excel_reactive$file_read) %>% all() 
      
      excel_reactive$missing_names <- setdiff(excel_reactive$nume_obligatorii,names(excel_reactive$file_read))
      
      excel_reactive$names_to_confirm <-  setdiff(names(excel_reactive$file_read),excel_reactive$nume_obligatorii)
       
       
      
      
      
    })
    
    
    
    
  }) 
  

}
    
## To be copied in the UI
# mod_read_excel_colaterale_ui("read_excel_colaterale_ui_1")
    
## To be copied in the server
# callModule(mod_read_excel_colaterale_server, "read_excel_colaterale_ui_1")
 
