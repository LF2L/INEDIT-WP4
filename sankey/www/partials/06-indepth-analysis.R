# Funcion for In depth analysis excel

In.depth.analysis <- function(Excel, Partner = "A definir", section1=c(2:7),  section2=c(8:20)  , ...) {
   # Naming the tables
   #Excel = INEDIT
   sheets.name = paste0('Sheet-', 1:length(Excel))
   Excel = Excel %>% set_names(sheets.name) 
   
   # Creating a only dataframe I
   Test = Excel %>% magrittr::extract(c(paste0('Sheet-', section1))) %>%  reduce(rbind)
   
   # Creating a only dataframe I
   Test2 = Excel %>% magrittr::extract(c(paste0('Sheet-', section2))) %>%  reduce(rbind) %>% mutate(Task = c("")) %>%
      select(Process, Activity, Task, "Involved stakeholders" : Practices)
   
   # Merge the two dataframes
   Test = rbind(Test, Test2) %>% mutate(Partner = Partner)
   rm(Test2)
   
   # Deleting the NA Values
   Test = Test %>% drop_na(Input)
   
   # Filling the gaps
   Test = Test %>% fill(Process, Activity)
   
   
   return(Test)
}