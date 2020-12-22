# This file contains some functions that are used all over the application: dt_generate_function - generates a nice datatable

dt_generate_function <- function(df,shape = "compact",round_col = NULL,perc_col = NULL,caption = "",editable=FALSE,
                  digits=0,show_buttons=FALSE,digits_perc=1) {
  result <- DT::datatable(class = shape,data = df,rownames = FALSE, caption = caption,editable=editable,extensions = "Buttons",
                          options = list(buttons=c("copy","excel"),scrollX=TRUE,dom = ifelse(show_buttons,'Bfrtip',"t"),info=FALSE,paging=FALSE,searching=FALSE, 
                                         columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
     DT::formatStyle(color = "#77547a",columns = 1:ncol(df))
  
  
  if (!is.null(round_col) & is.null(perc_col)) {return(result %>% 
                    DT::formatRound(columns = round_col,digits = digits,dec.mark = ".") ) }
  
  else if (is.null(round_col) & is.null(perc_col)) { return(result)  }
  
  else if (!is.null(round_col) & !is.null(perc_col)) {return(result %>% 
      DT::formatRound(columns = round_col,digits = digits) %>%
      DT::formatPercentage(columns = perc_col,digits = digits_perc) ) }
  else {return( result %>% DT::formatPercentage(columns = perc_col,digits = digits) )  }
  
}
  