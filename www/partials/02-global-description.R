# Funcion para color la description of the case study
global.description <- function(Test, ...) {
   
   Description =   
      tribble(~" ", ~Description ,
              "Title" , Test$Title, 
              "Role" , Test$Role,
              "Presentation" , c(Test$Presentation) ,      
              "Objective" , Test$Objective
      )
   
 table =  
    Description %>%
      kable() %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = T ) %>%
      column_spec(1, bold = T, border_right = T, width = "10em")  %>%
      collapse_rows(columns = 1, valign = "middle")
   
return(table)
}