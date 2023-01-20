# Date of the creation of the document: Friday 19/2022

# Loading the package
library(httr)
library(readxl)
library(here)
library(tidyverse)

# the URL of your sharepoint file -----
#file_url <- "https://ensameu.sharepoint.com/:x:/r/sites/INEDITProject/Documents%20partages/WP4%20Documents/T4.3%20Technological%20and%20organizational%20development%20of%20the%20future%20use%20cases/monitoring/UL-Monitoring_%26_Validation_UC-V.2021-09-28.xlsx?d=wc1bb3ba4c27c42569f7af65fdb7776cb&csf=1&web=1&e=QkMBB4"


# Reading Excel File ----
files <- here("data", "dashboard", list.files("data/dashboard")) 
excel <- files[1]
T4.3_sheets <- excel_sheets(excel) # Names of the Sheets of the documents

# Reading and creating a list from the Excel
T4.3 <- 
   lapply(excel_sheets(path=excel), read_excel, path = excel) %>% 
   set_names(excel_sheets(path=excel))


# Creating the Nested dataframe  with only the Functions ----
T4.3_functions <- 
   T4.3_sheets[3:12]  %>%  
   map( ~ read_excel(path = excel, sheet = . ,   skip = 2)) %>%  # Reading the only data structure
   set_names(T4.3_sheets[3:12]) %>% 
   reduce(rbind) %>%  # Reducing to one only dataframe
   fill("Function", "Sub-function", "Criterion") %>% # filling the gaps
   filter(Flexiblity == "F0" | Flexiblity == "F1/F2")


# Exporting CSVs --------

## Exporting the  Functions and Subfunctions ------
Fun_sub <- T4.3_functions %>% select(Function, `Sub-function`) %>% unique()
#write_csv2(x= Fun_sub, file = "data/dashboard/UL-Monitoring-Delivrable-Functions-Subfunctions.csv")

## Exporting the  Functions, Subfunctions  and Criteria ------
Fun_sub_crit <- T4.3_functions %>% select(Function, `Sub-function`, Criterion) %>% unique()
#write_csv2(x= Fun_sub_crit, file = "data/dashboard/UL-Monitoring-Delivrable-Functions_Subfunctions_criterion.csv")


Test <- 
   sankey %>% select(Function, `Sub-function`, Criterion, Recovery) %>% 
   mutate(Value = 1 ) %>% 
   drop_na(Recovery) %>%
   select(-Recovery)


A <- Test %>%
   select(Function, `Sub-function`) %>%
   group_by(Function,`Sub-function`) %>% 
   summarise (Value = n() ) %>% 
   set_names(c("Source", "Target", "Value")) 

B <- Test %>%
   select(`Sub-function`, Criterion) %>%
   group_by(`Sub-function`, Criterion) %>% 
   summarise (Value = n() ) %>% 
   set_names(c("Source", "Target", "Value")) 


Test <- rbind(A,B)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame( name=c(as.character(Test$Source), as.character(Test$Target)) %>% unique())
nodes$width <- rep(c(100,20), 23)

#length(?gregexpr(" ", "I am going Out")[[1]])


# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
Test$IDsource <- match(Test$Source, nodes$name)-1 
Test$IDtarget <- match(Test$Target, nodes$name)-1

network <- 
   sankeyNetwork(Links = Test, Nodes = nodes,
                 Source = "IDsource", Target = "IDtarget", 
                 Value = "Value", NodeID = "name",  
                 fontSize = 15,
                 #nodeWidth = nodes$width,
                 LinkGroup="Source",# NodeGroup="Group",
                 sinksRight=FALSE,
                 nodePadding = 1,
                 height = 800,
                 width = 1000
   )

network




library(networkD3)
library(htmlwidgets)
library(data.table)

set.seed(1999)
links <- data.table(
   src = rep(0:4, times=c(1,1,2,3,5)),
   target = sample(1:11, 12, TRUE),
   value = sample(100, 12)
)[src < target, ]  # no loops
nodes <- data.table(name=LETTERS[1:12])

## Add text to label
txt <- links[, .(total = sum(value)), by=c('target')]
nodes[txt$target+1L, name := paste0(name, '<br>(', txt$total, ')')]

## Displays the counts as part of the labels
sn <- sankeyNetwork(Links=links, Nodes=nodes, Source='src', Target='target',
                    Value='value', NodeID='name', fontSize=16, nodeWidth=50, width=600, height=300)



# PREPARATION ----

Test <- tibble(
      Source = c("Test", "Test\n2 in function"),
       Target = c('Valor<br>largo\nlarog', "valor"),
       Value = c(1,2))

Test
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame( name=c(as.character(Test$Source), as.character(Test$Target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
Test$IDsource <- match(Test$Source, nodes$name)-1 
Test$IDtarget <- match(Test$Target, nodes$name)-1


simpleNetwork(Data=Test, 
              height="100px", width="100px")



sn <- 
sankeyNetwork(Links = Test, Nodes = nodes,
                 Source = "IDsource", Target = "IDtarget", 
                 Value = "Value", NodeID = "name",  
                 fontSize = 15,
                 LinkGroup="Source",# NodeGroup="Group",
                 sinksRight=FALSE,
                 nodeWidth = 5,
                 nodePadding = 1,
                 height = 100,
                 width = 300
   )


onRender(sn,
         '
  function(el,x) {
    d3.selectAll(".node text").remove()
    d3.selectAll(".node")
      .append("foreignObject")
      .attr("width", 400)
      .attr("height", 250)
      .html(function(d) { return d.name; })
  }
  '
)


# Exporting Data
# write_csv2(x=T4.3_functions %>% select(Criterion, Control:Comment),
#            file = "data/dashboard/UL-Monitoring-Delivrable.csv")


# Separating the columns
names(T4.3_functions)
Data <- 
   T4.3_functions %>% 
   separate(col = `Sub-function`, into = c("sub_function", "Inedit"), sep = "WP2.2:" )


# Doing the Graph
names(Data)
Test <- Data %>% select(Function, Inedit) %>% mutate(Value = 1 ) %>% set_names(c("Source", "Target", "Value")) 




# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame( name=c(as.character(Test$Source), as.character(Test$Target)) %>% unique())
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
Test$IDsource <- match(Test$Source, nodes$name)-1 
Test$IDtarget <- match(Test$Target, nodes$name)-1

network <- 
   sankeyNetwork(Links = Test, Nodes = nodes,
                 Source = "IDsource", Target = "IDtarget", 
                 Value = "Value", NodeID = "name",  fontSize = 15, #LinkGroup="Group",# NodeGroup="Group",
                 sinksRight=FALSE,
                 nodeWidth = 30,
                 height = 1500,
                 width = 1500,
                 nodePadding = 1
   )

saveNetwork(network, "test.html", selfcontained = TRUE)
network

X = Test %>% select(Partner, Process ) %>% mutate(Value = 1 ) %>% set_colnames(c("Source", "Target", "Value")) 
Y = Test %>% select(Process, Activity ) %>% mutate(Value = 1 ) %>% set_colnames(c("Source", "Target", "Value")) 
#Z = Test %>% select(Activity, Task ) %>% mutate(Value = 1 ) %>% set_colnames(c("Source", "Target", "Value")) 

Test = rbind(X,Y) 
rm(X,Y)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame( name=c(as.character(Test$Source), as.character(Test$Target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
Test$IDsource <- match(Test$Source, nodes$name)-1 
Test$IDtarget <- match(Test$Target, nodes$name)-1
