#' read_excel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_read_excel_ui <- function(id){
  ns <- NS(id)
}
    
#' read_excel Server Function
#'
#' @noRd 
mod_read_excel_server <- function(input, output, session,excel_reactive){
  # gets a reactiveValues() list with compontet file_input
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
      readxl::read_excel(excel_reactive$file_input,sheet = selected_sheet(), range = "A1:AA50",.name_repair = "minimal")  })
  
    
    # I get the row index where name if the columns are
    index_citire <- reactive({ req(excel_first_read())
      apply(excel_first_read(),1,function(x) (sum(excel_reactive$nume_obligatorii %in% x)==length(excel_reactive$nume_obligatorii))) %>% 
        which(TRUE) %>% max(0,na.rm = TRUE)})
    
    # Second read of the excel, this time starting where the column names are
    file_read <- reactive({req(excel_first_read())
      readxl::read_excel(excel_reactive$file_input,sheet = selected_sheet(), skip = index_citire())   })
    #observe({req(index_citire())
      #excel_reactive$file_read <-  readxl::read_excel(excel_reactive$file_input,sheet = selected_sheet(), skip = index_citire()) })
   
    
    observe({req(file_read())
      excel_reactive$all_names <- excel_reactive$nume_obligatorii %in% names(file_read()) %>% all() 
      
      excel_reactive$missing_names <- setdiff(excel_reactive$nume_obligatorii,names(file_read()))
      
      if (excel_reactive$all_names) {
        
        if (is.null(excel_reactive$column_names_date)) {
          excel_reactive$column_types_date <- NULL }
        
        else { excel_reactive$column_types_date <- purrr::map_chr(names(file_read()),
                                ~ifelse(.x %in% excel_reactive$column_names_date,"date","guess"))}
        
        
        excel_reactive$file_read_prel <- readxl::read_excel(excel_reactive$file_input,sheet = selected_sheet(), 
                skip = index_citire(),col_types = excel_reactive$column_types_date) %>%
                  dplyr::select(excel_reactive$nume_obligatorii) %>%
                    dplyr::mutate_if(.predicate = lubridate::is.POSIXct,.funs = as.Date.POSIXct) %>%
                      dplyr::mutate_if(.predicate = lubridate::is.POSIXlt,.funs = as.Date.POSIXlt)
          }
      
    })
    
    
    
    
  }) 
  
}
    
## To be copied in the UI
# mod_read_excel_ui("read_excel_ui_1")
    
## To be copied in the server
# callModule(mod_read_excel_server, "read_excel_ui_1")
 
