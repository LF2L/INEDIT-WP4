# Funcion for the matrix impact plot


matrix.impact <- function(matrix, ...) {
   
   impact.df = c()
   for (i in 1:length(matrix)) {
      
      partial = matrix[i] %>% melt() #  %>% add_column(Dimension = NA)   
      impact.df = rbind(impact.df, partial)
   }
   
   
   # Adding the colu
   impact.df = impact.df  %>% add_column(valor = 0)
   impact.df = impact.df %>% mutate(valor = replace(valor, value== "TRUE", 2))
   
   # Ordering the factors
   niv = impact.df %>% filter(L1 == "Governance") %>% select(L3)
   
   impact.df$L3 = as.factor(impact.df$L3)
   impact.df$L3 = factor(impact.df$L3, levels= niv$L3)
   
   # Adding Definitions
   Definitions =   
      tribble(~L1, ~Description ,
              "Business Model", "Describes an innovative and profitable business model, ensuring the sustainability or deployment of pilot projects" , 
              "Governance", "Promotes new forms of multi-actor design and management, facilitating public procurement and helping public purchasers to make better strategic use of it.",
              "Integrated urban service", "Optimizes the delivery of new or existing services through synergies between infrastructures, pooling of resources, systems and data",
              "New or existing sinergy" , "Allows new actions to contribute to the improvement of existing ones",
              "Prospective opening", "Meets emerging or anticipated needs, contributing to the resilience of the territory ",
              "Relationship to the territory", "Favours the development of actions, local know-how or services favourable to the territory as a whole, making it more attractive",
              "Social and organizational innovations", "Promotes the emergence of new forms of public action and collaboration between actors",
              "Technical or technological innovations" , "Marks a break/significant advance compared to current projects", 
              "Visibility and recognition", "Give a potentially strong image of the project and gives credibility to the prospect of its export."
      )
   
   data = merge(impact.df, Definitions, by="L1" )
   
   data = na.omit(data)
   
   # Crando la figura
   figure =
      ggplot(data , aes(x=L3, y=L1)) +
      geom_blank()+
      ggplot2::annotate("rect", xmin=c(1,4,7,10,12), xmax=c(3,6,9,11,14), 
                        ymin=rep(1,5), ymax=rep(10, 5), 
                        alpha = .1 , fill = c("blue", "red", "black","green", "gold")) +
      ggplot2::annotate("text",
                        x = c(2,5,8,10,13),
                        y = rep(10, 5),
                        #hjust = 1,
                        label = c("Energy /\n Climate","Ecosystemics", "Uses", "Economical", "Creativity"),
                        family = "Palatino", fontface = 3, size=4) +
      geom_point( aes(text = paste(paste("Definition: ", Description,  sep="\n"), sep="\n" )  , size = valor),  alpha=0.7, color = "#00AFBB") +
      scale_size_continuous(range = c(-1, 5)) +
      theme_minimal(base_size = 12, base_family = "Palatino") +
      theme(
         legend.position = "none",
         axis.text.x = element_text(face = "bold", color = "dodgerblue4", size = 12, angle = 90),
         axis.text.y = element_text(face = "bold", color = "dodgerblue4", size = 12),
         panel.border = element_blank(),
         panel.spacing = unit(0.1, "lines"),
         strip.text.x = element_text(size = 12, family = "Palatino")
      ) +
      labs(x = "", y = " ", title = "Impact matrix" ) +
      coord_cartesian(ylim = c(1, 11))

      
   matrix =  ggplotly( figure, height=500, width=1000,  tooltip=c('text')) # "Name" or "University"
   
     
   return(matrix)
}