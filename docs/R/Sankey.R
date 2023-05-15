# Function de Sankey

Sankey_inedit <- 
   function(Test, Source, Target){
   
   # From these flows we need to create a node data frame: it lists every entities involved in the flow
   nodes <- data.frame( name=c(as.character(Test$Source), as.character(Test$Target)) %>% unique())
   
   
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
   
   return(network)
   }