## Date of creation of the document: 11/01/2023

# Loading the package
library(httr)
library(readxl)
library(here)
library(tidyverse)
library(networkD3)

# the URL of your sharepoint file -----
#file_url <- "https://ensameu.sharepoint.com/:x:/r/sites/INEDITProject/Documents%20partages/WP4%20Documents/T4.3%20Technological%20and%20organizational%20development%20of%20the%20future%20use%20cases/monitoring/UL-Monitoring_%26_Validation_UC-V.2021-09-28.xlsx?d=wc1bb3ba4c27c42569f7af65fdb7776cb&csf=1&web=1&e=QkMBB4"


# Reading Excel File ----
files <- here("data", "dashboard", list.files("data/dashboard")) 
T4.3_sheets <- excel_sheets(files[1]) # Names of the Sheets of the documents

## Reading and creating a list from the Excel ----
T4.3 <- 
   lapply(excel_sheets(path=files[1]), read_excel, path = files[1]) %>% 
   set_names(excel_sheets(path=files[1]))

T4.3 <- 
   T4.3_sheets[3:12]  %>%  
   map( ~ read_excel(path = files[1], sheet = . ,   skip = 2)) %>%  # Reading the only data structure
   set_names(T4.3_sheets[3:12]) %>% 
   reduce(rbind) %>%  # Reducing to one only dataframe
   fill("Function", "Sub-function", "Criterion")

## Vectors of the Functions, Subfonctions ----
Names <- list()
Names$Functions <- T4.3 %>% select("Function") %>% unique()
Names$Subfunctions <- T4.3 %>% select("Sub-function") %>% unique()
Names$Criterion <- T4.3 %>% select("Criterion") %>% unique()

Names$Functions


# Green Fablab  Functions ----
GF <- T4.3 %>% filter(Flexiblity == "F0" | Flexiblity == "F1/F2")


## Exporting CSVs --------
## Exporting the  Functions and Subfunctions ------
GF_Fun_sub <- GF %>% select(Function, `Sub-function`) %>% unique()
#write_csv2(x= Fun_sub, file = "data/dashboard/UL-Monitoring-Delivrable-Functions-Subfunctions.csv")

## Exporting the  Functions, Subfunctions  and Criteria ------
GF_Fun_sub_crit <- GF %>% select(Function, `Sub-function`, Criterion) %>% unique()
#write_csv2(x= Fun_sub_crit, file = "data/dashboard/UL-Monitoring-Delivrable-Functions_Subfunctions_criterion.csv")



# Green Fablab Global  -----
## Reading data -----
GF_global <- read_csv2(file= files[2]) 

## Order the factors
GF_global <- 
   GF_global %>% 
   mutate(
      Function = factor(Function, 
                        levels=c(
                           "PF1: Design a customized product for users with the help of other specialized stakeholders",                   
                           "PF3: Produce a customized product for users with the help of other specialized stakeholders",                  
                           "PF4: Produce in network with other stakeholders using the platform",                                           
                           "PF5: Provide services to users through the platform",                                                          
                           "SF1: Provide manufacturing data (means of production, production capacity, location, delays…) to the platform",
                           "SF2: Share environmental practices to the other stakeholders",                                                 
                           "SF3: Share manufacturing data for design to the users",                                                        
                           "SF4: Insert into the territory",                                                                               
                           "CF1: Adjust to the pillars of the circular economy"
                        ),
                        labels = c(
                           "PF1",
                           "PF3",
                           "PF4",
                           "PF5",
                           "SF1",
                           "SF2",
                           "SF3",
                           "SF4",
                           "CF1"
                           # "PF1: Design a customized product for users\nwith the help of other specialized stakeholders",                   
                           # "PF3: Produce a customized product for users\nwith the help of other specialized stakeholders",                  
                           # "PF4: Produce in network with other\nstakeholders using the platform",                                           
                           # "PF5: Provide services to\nusers through the platform",                                                          
                           # "SF1: Provide manufacturing data\n(means of production, production capacity, location, delays…)\nto the platform",
                           # "SF2: Share environmental practices\nto the other stakeholders",                                                 
                           # "SF3: Share manufacturing data\nfor design to the users",                                                        
                           # "SF4: Insert into the territory",                                                                               
                           # "CF1: Adjust to the pillars of\nthe circular economy"                       
                        ))

      )

## Ordering the factors for Sub

subf_levels <- GF_global %>% group_by(Function, `Sub-function`) %>% summarise(n = n()) %>% pull(`Sub-function`)

GF_global$`Sub-function` <- factor(GF_global$`Sub-function`,
                                   levels = subf_levels)

rm(subf_levels)


## Ordering factors of DRAM

GF_global <- 
   GF_global %>%
   pivot_longer(cols = Recovery:Plateform, names_to = "DRAM", values_to = "Value") %>% 
   drop_na(Value) %>% 
   mutate(
      DRAM = factor(DRAM, 
                    levels=c("Recovery", "Preparation", "Compounding", "Feedstock", "Printing","Quality","Plateform" )),
   )


 ## Graphique Sankey

Sankey_1 <- 
   GF_global %>% 
   #filter(DRAM == "Recovery") %>%
   group_by(Function, DRAM) %>% 
   summarise(Value = n()) %>% 
   # mutate(  Value = case_when(
   #    DRAM == "Recovery" ~ as.numeric(Value),
   #    TRUE ~ 0  )) %>% 
   set_names("Source", "Target", "Value")

source('R/Sankey.R')
Sankey_inedit(Sankey_1)



### Alluvial ----
library(ggalluvial)

Sankeys <- list()

Sankeys$Global <-
   GF_global %>% 
   group_by(Function, DRAM) %>% 
   summarise(Value = n()) 
   

Test <-  Sankeys$Global
Test %>% 
   ggplot(aes(axis1 = Function, axis2 = DRAM, y = Value)) +
   scale_x_discrete(limits = c("Functions", "Green Fablab"), expand = c(.2, .05)) +
   geom_alluvium(aes(fill = Function)) +
#   geom_stratum() + 
   geom_stratum(width= c(rep(0.3, 9), rep(0.4, 7) ))+
   geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
   #coord_cartesian(xlim = c(-1, 5)) +
   scale_fill_discrete(name = "OMDF Functions", 
                       labels = c(
                          "PF1: Design a customized product for users\nwith the help of other specialized stakeholders",                   
                          "PF3: Produce a customized product for users\nwith the help of other specialized stakeholders",                  
                          "PF4: Produce in network with other\nstakeholders using the platform",                                           
                          "PF5: Provide services to\nusers through the platform",                                                          
                          "SF1: Provide manufacturing data\n(means of production, production capacity, location, delays…)\nto the platform",
                          "SF2: Share environmental practices\nto the other stakeholders",                                                 
                          "SF3: Share manufacturing data\nfor design to the users",                                                        
                          "SF4: Insert into the territory",                                                                               
                          "CF1: Adjust to the pillars of\nthe circular economy"                                                 
                       )) +
   theme_minimal(base_size = 14,  base_family = "Palatino") +
   theme(#legend.position = 'top', 
      legend.spacing.y = unit(0.2, 'cm'))+
   guides(fill = guide_legend(byrow = TRUE)) +
   labs(title = "Open Manufacturing Demonstration Facilities (OMDF)",
        subtitle = "Connected to the Green Fablab Demostrator",
        x = "",
        #x = "Connection of OMDF fonctions with the Demostrator",
        y = "Quantity of Criteria"
   )


## Exporting the file
#ggsave(filename = "Sankey-GF-Global.jpg", path= "Figures/", width = 12, height = 8 )



# Detailing DRAM Functions -----
## Recovery ------
Sankeys$Recovery <-
   GF_global %>% filter(DRAM == "Recovery") %>% 
   group_by(Function, `Sub-function`) %>% 
   summarise(Value = n()) 


Test <-  Sankeys$Recovery %>% mutate(DRAM = "Recovery")
Test %>% 
   ggplot(aes(axis1 = Function, axis2 = DRAM, axis3 = `Sub-function`, y = Value)) +
   scale_x_discrete(limits = c("Functions", "Green Fablab", "Sub functions"), expand = c(0.05, 0.4)) +
   geom_alluvium(aes(fill = Function)) +
   geom_stratum(width= 1/3) +
   #geom_stratum(width= c(rep(0.3, 9), rep(0.4, 7) ))+
   geom_text(stat = "stratum", nudge_x = c(rep(-0.15, 7), rep(0.2, 15)) ,
             aes(label = after_stat(stratum), 
                 #hjust = 0
                 #hjust = c(rep(0, 12), rep(0, 3)) 
                 hjust = c(rep(0, 1), rep(0, 14)) 
                 ),
             
             ) +
   coord_cartesian(xlim = c(1, 10)) +
   scale_fill_discrete(name = "Functions of Green Fablab", 
                       labels = c(
                          "PF1: Design a customized product for users\nwith the help of other specialized stakeholders",                   
                          "PF3: Produce a customized product for users\nwith the help of other specialized stakeholders",                  
                          "PF4: Produce in network with other\nstakeholders using the platform",                                           
                          "PF5: Provide services to\nusers through the platform",                                                          
                          "SF1: Provide manufacturing data\n(means of production, production capacity, location, delays…)\nto the platform",
                          "SF2: Share environmental practices\nto the other stakeholders",                                                 
                          "SF3: Share manufacturing data\nfor design to the users",                                                        
                          "SF4: Insert into the territory",                                                                               
                          "CF1: Adjust to the pillars of\nthe circular economy"                                                 
                       )) +
   theme_minimal(base_size = 14,  base_family = "Palatino") +
   theme(legend.position = "bottom",
      legend.spacing.y = unit(0.2, 'cm')
      )+
   #guides(fill = guide_legend(byrow = TRUE)) +
   labs(title = "INEDIT Functions",
        subtitle = "Connected to the Green Fablab Demostrator",
        x = "Connection of INEDIT Fonctions with the Demostrator",
        y = "Quantity of Criteria"
   )


## Exporting the file
#ggsave(filename = "Sankey-GF-Recovery.jpg", path= "Figures/", width = 15, height = 7 )




## Preparation ------
Sankeys$Preparation <-
   GF_global %>% filter(DRAM == "Preparation") %>% 
   group_by(Function, `Sub-function`) %>% 
   summarise(Value = n()) 


Test <-  Sankeys$Preparation %>% mutate(DRAM = "Preparation")


Test %>% 
   ggplot(aes(axis1 = Function, axis2 = DRAM, axis3 = `Sub-function`, y = Value)) +
   scale_x_discrete(limits = c("Functions", "Green Fablab", "Sub functions"), expand = c(.2, .05)) +
   geom_alluvium(aes(fill = Function)) +
   geom_stratum(width= 0.4) +
   #geom_stratum(width= c(rep(0.3, 9), rep(0.4, 7) ))+
   geom_text(stat = "stratum", nudge_x = -0.1 , 
             aes(label = after_stat(stratum),
                 #hjust = 0
                 #hjust = c(rep(0, 12), rep(0, 3)) 
                 hjust = 0
                 )) +
   coord_cartesian(xlim = c(0, 10)) +
   scale_fill_discrete(name = "Functions of Green Fablab", 
                       labels = c(
                          "PF1: Design a customized product for users\nwith the help of other specialized stakeholders",                   
                          "PF3: Produce a customized product for users\nwith the help of other specialized stakeholders",                  
                          "PF4: Produce in network with other\nstakeholders using the platform",                                           
                          "PF5: Provide services to\nusers through the platform",                                                          
                          "SF1: Provide manufacturing data\n(means of production, production capacity, location, delays…)\nto the platform",
                          "SF2: Share environmental practices\nto the other stakeholders",                                                 
                          "SF3: Share manufacturing data\nfor design to the users",                                                        
                          "SF4: Insert into the territory",                                                                               
                          "CF1: Adjust to the pillars of\nthe circular economy"                                                 
                       )) +
   theme_minimal(base_size = 14,  base_family = "Palatino") +
   theme(legend.position = "bottom",
         legend.spacing.y = unit(0.2, 'cm')
   )+
   #guides(fill = guide_legend(byrow = TRUE)) +
   labs(title = "INEDIT Functions",
        subtitle = "Connected to the Green Fablab Demostrator",
        x = "Connection of INEDIT Fonctions with the Demostrator",
        y = "Quantity of Criteria"
   )


#Exporting the figure
# ggsave(filename = "Sankey-GF-Preparation.jpg", path= "Figures/", width = 16, height = 8 )




## Compounding ------
Sankeys$Compounding <-
   GF_global %>% filter(DRAM == "Compounding") %>% 
   group_by(Function, `Sub-function`) %>% 
   summarise(Value = n()) 

Test <-  Sankeys$Compounding %>% mutate(DRAM = "Compounding")


Test %>% 
   ggplot(aes(axis1 = Function, axis2 = DRAM, axis3 = `Sub-function`, y = Value)) +
   scale_x_discrete(limits = c("Functions", "Green Fablab", "Sub functions"), expand = c(0, 0.01)) +
   geom_alluvium(aes(fill = Function)) +
   geom_stratum(width= 0.4) +
   #geom_stratum(width= c(rep(0.3, 9), rep(0.4, 7) ))+
   geom_text(stat = "stratum", nudge_x = -0.1 , 
             aes(label = after_stat(stratum),
                 #hjust = 0
                 hjust = c(0, 0 , 0 , 0, 0 ) 
                 #hjust = 0
             )) +
   coord_cartesian(xlim = c(0, 10)) +
   scale_fill_discrete(name = "Functions of Green Fablab", 
                       labels = c(
                          "PF1: Design a customized product for users\nwith the help of other specialized stakeholders",                   
                          "PF3: Produce a customized product for users\nwith the help of other specialized stakeholders",                  
                          "PF4: Produce in network with other\nstakeholders using the platform",                                           
                          "PF5: Provide services to\nusers through the platform",                                                          
                          "SF1: Provide manufacturing data\n(means of production, production capacity, location, delays…)\nto the platform",
                          "SF2: Share environmental practices\nto the other stakeholders",                                                 
                          "SF3: Share manufacturing data\nfor design to the users",                                                        
                          "SF4: Insert into the territory",                                                                               
                          "CF1: Adjust to the pillars of\nthe circular economy"                                                 
                       )) +
   theme_minimal(base_size = 14,  base_family = "Palatino") +
   theme(legend.position = "bottom",
         legend.spacing.y = unit(0.2, 'cm')
   )+
   #guides(fill = guide_legend(byrow = TRUE)) +
   labs(title = "INEDIT Functions",
        subtitle = "Connected to the Green Fablab Demostrator",
        x = "Connection of INEDIT Fonctions with the Demostrator",
        y = "Quantity of Criteria"
   )


#Exporting the figure
# ggsave(filename = "Sankey-GF-Compounding.jpg", path= "Figures/", width = 16, height = 8 )




## Feedstock ------
Sankeys$Feedstock <-
   GF_global %>% filter(DRAM == "Feedstock") %>% 
   group_by(Function, `Sub-function`) %>% 
   summarise(Value = n()) 

Test <-  Sankeys$Feedstock %>% mutate(DRAM = "Feedstock")

Test %>% 
   ggplot(aes(axis1 = Function, axis2 = DRAM, axis3 = `Sub-function`, y = Value)) +
   scale_x_discrete(limits = c("Functions", "Green Fablab", "Sub functions"), expand = c(0, 0.01)) +
   geom_alluvium(aes(fill = Function)) +
   geom_stratum(width= 0.4) +
   #geom_stratum(width= c(rep(0.3, 9), rep(0.4, 7) ))+
   geom_text(stat = "stratum", nudge_x = -0.1 , 
             aes(label = after_stat(stratum),
                 #hjust = 0
                 hjust = c(0, 0 , 0 , 0, 0 ) 
                 #hjust = 0
             )) +
   coord_cartesian(xlim = c(0, 10)) +
   scale_fill_discrete(name = "Functions of Green Fablab", 
                       labels = c(
                          "PF1: Design a customized product for users\nwith the help of other specialized stakeholders",                   
                          "PF3: Produce a customized product for users\nwith the help of other specialized stakeholders",                  
                          "PF4: Produce in network with other\nstakeholders using the platform",                                           
                          "PF5: Provide services to\nusers through the platform",                                                          
                          "SF1: Provide manufacturing data\n(means of production, production capacity, location, delays…)\nto the platform",
                          "SF2: Share environmental practices\nto the other stakeholders",                                                 
                          "SF3: Share manufacturing data\nfor design to the users",                                                        
                          "SF4: Insert into the territory",                                                                               
                          "CF1: Adjust to the pillars of\nthe circular economy"                                                 
                       )) +
   theme_minimal(base_size = 14,  base_family = "Palatino") +
   theme(legend.position = "bottom",
         legend.spacing.y = unit(0.2, 'cm')
   )+
   #guides(fill = guide_legend(byrow = TRUE)) +
   labs(title = "INEDIT Functions",
        subtitle = "Connected to the Green Fablab Demostrator",
        x = "Connection of INEDIT Fonctions with the Demostrator",
        y = "Quantity of Criteria"
   )


#Exporting the figure
# ggsave(filename = "Sankey-GF-Feedstock.jpg", path= "Figures/", width = 16, height = 8 )





## Printing ------
Sankeys$Printing <-
   GF_global %>% filter(DRAM == "Printing") %>% 
   group_by(Function, `Sub-function`) %>% 
   summarise(Value = n()) 

Test <-  Sankeys$Printing %>% mutate(DRAM = "Printing")

Test %>% 
   ggplot(aes(axis1 = Function, axis2 = DRAM, axis3 = `Sub-function`, y = Value)) +
   scale_x_discrete(limits = c("Functions", "Green Fablab", "Sub functions"), expand = c(0, 1)) +
   geom_alluvium(aes(fill = Function)) +
   geom_stratum(width= 0.4) +
   #geom_stratum(width= c(rep(0.3, 9), rep(0.4, 7) ))+
   geom_text(stat = "stratum", nudge_x = -0.1 , 
             aes(label = after_stat(stratum),
                 #hjust = 0
                 hjust = 0
                 #hjust = 0
             )) +
   coord_cartesian(xlim = c(0, 10)) +
   scale_fill_discrete(name = "Functions of Green Fablab", 
                       labels = c(
                          "PF1: Design a customized product for users\nwith the help of other specialized stakeholders",                   
                          "PF3: Produce a customized product for users\nwith the help of other specialized stakeholders",                  
                          "PF4: Produce in network with other\nstakeholders using the platform",                                           
                          "PF5: Provide services to\nusers through the platform",                                                          
                          "SF1: Provide manufacturing data\n(means of production, production capacity, location, delays…)\nto the platform",
                          "SF2: Share environmental practices\nto the other stakeholders",                                                 
                          "SF3: Share manufacturing data\nfor design to the users",                                                        
                          "SF4: Insert into the territory",                                                                               
                          "CF1: Adjust to the pillars of\nthe circular economy"                                                 
                       )) +
   theme_minimal(base_size = 14,  base_family = "Palatino") +
   theme(legend.position = "bottom",
         legend.spacing.y = unit(0.2, 'cm')
   )+
   #guides(fill = guide_legend(byrow = TRUE)) +
   labs(title = "INEDIT Functions",
        subtitle = "Connected to the Green Fablab Demostrator",
        x = "Connection of INEDIT Fonctions with the Demostrator",
        y = "Quantity of Criteria"
   )


#Exporting the figure
# ggsave(filename = "Sankey-GF-Printing.jpg", path= "Figures/", width = 16, height = 9 )















