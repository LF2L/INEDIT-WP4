# Funcion para color la description of the case study
innovations <- function(Test, ...) {
   
   Description =  Test$Innovations %>% melt() %>% filter(L2 == "Description") %>% 
                     select(L1,value) %>% set_names ("Innovation", "Description")
   
   
   table =  
      Description %>%
      kable( row.names = TRUE) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = T ) %>%
      column_spec(2, bold = T, border_right = T, width = "10em")  %>%
      collapse_rows(columns = 1, valign = "middle")
   
   return(table)
}