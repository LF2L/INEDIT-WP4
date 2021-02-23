# Funcion for the matrix impact plot

global.matrix.impact <- function(matrix, ...) {
   
   impact.df = c()
   
   for (i in 1:length(matrix)) {
      partial = matrix[i] %>% melt() #  %>% add_column(Dimension = NA)   
      impact.df = rbind(impact.df, partial)
   }
   
   
   # Adding the column Value
   impact.df <- tibble(impact.df)
   impact.df <- impact.df  %>% add_column(valor = 0)
   impact.df <- impact.df %>% mutate(valor = replace(valor, value == "TRUE", 1))
   
   # Creating the Data Global for INEDIT
   global <- impact.df %>% group_by(L4, L3, L2) %>% summarise(valor = sum(valor)) %>% mutate(L1 = "Global analysis")
   global <- global %>% select(L4, L3, L2, L1, valor)
   global <- rbind(global, impact.df %>% select(L4, L3, L2, L1, valor) )
   
   
   # Ordering the factors
   niv <- global %>% filter(L2 == "Governance" & L1 == "AIMEN"  ) %>% select(L4)
   
   global$L4 <- as.factor(global$L4)
   global$L4 <- factor(global$L4, levels= niv$L4)
   
   # Adding Definitions
   Definitions =   
      tribble(~L2, ~Description ,
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
   
   #data = merge(global, Definitions, by="L2" )
   data <- global %>% left_join(Definitions, by="L2")
   data <- na.omit(data)
   data <- tibble(data)
   data$L1 = factor(data$L1, levels= c("Global analysis", "AIMEN", "UL", "UNNINOVA", "VERA"))
   
   #Arranging the factors
   data$L2 %>% levels()
   
   data$L2 <- factor(data$L2,
                     levels= c(
                        "Visibility and recognition",
                        "Prospective opening",
                        "Integrated urban service",
                        "New or existing sinergy",
                        "Relationship to the territory",
                        "Governance",
                        "Business Model",
                        "Social and organizational innovations",
                        "Technical or technological innovations"
                     ),
                     labels= c(
                        "Visibility\nand\nrecognition",
                        "Prospective\nopening",
                        "Integrated\nurban\nservice",
                        "New or\nexisting\nsinergy",
                        "Relationship to\nthe territory",
                        "Governance",
                        "Business\nModel",
                        "Social and\norganizational\ninnovations",
                        "Technical or\ntechnological\ninnovations"
                     )                     
                     )
   
   
   data$L4 <- factor(data$L4,
                     levels = 
                        c(
                           "Energy",
                           "Materials and life cycle",
                           "Sustainable mobility",
                           "Water management",
                           "Biodiversity",
                           "Waste / Material flow",
                           "Confort / Ambiance",
                           "Accommodation / Jobs / Service",
                           "Health / Safety",
                           "Bussiness model",
                           "Export",
                           "Culture",
                           "Digital",
                           "Participation"
                        ),
                     labels = 
                        c(
                           "Energy",
                           "Materials\nand\nlife cycle",
                           "Sustainable\nmobility",
                           "Water\nmanagement",
                           "Biodiversity",
                           "Waste/\nMaterial\nflow",
                           "Confort/\nAmbiance",
                           "Accommodation /\nJobs /\nService",
                           "Health/\nSafety",
                           "Bussiness\nmodel",
                           "Export",
                           "Culture",
                           "Digital",
                           "Participation"
                        )
                     )
   
   
   # Creando la figura
   figure  <-
      data %>% 
      ggplot(aes(x=L4, y=L2)) +
      geom_blank() +
      ggplot2::annotate("rect", xmin=c(1,4,7,10,12), xmax=c(3,6,9,11,14), 
                        ymin=rep(1,5), ymax=rep(10, 5), 
                        alpha = .1 , fill = c("blue", "red", "black","green", "gold")) +
      ggplot2::annotate("text",
                        x = c(2,5,8,10,13),
                        y = rep(10, 5),
                        #hjust = 1,
                        label = c("Energy / Climate","Ecosystemics", "Uses", "Economical", "Creativity"),
                        family = "Palatino", fontface = 3, size=4) +
      geom_point( aes(text = paste(paste("Definition: ", Description,  sep="\n"), sep="\n" ), color =L1, size = valor),  alpha=0.7) +
      scale_size_continuous(range = c(-1, 5)) +
      theme_minimal(base_size = 12, base_family = "Palatino") +
      theme(
         legend.position = "bottom",
         axis.text.x = element_text(face = "bold", color = "dodgerblue4", size = 10, angle = 0),
         axis.text.y = element_text(face = "bold", color = "dodgerblue4", size = 10),
         panel.border = element_blank(),
         panel.spacing = unit(0.9, "lines"),
         strip.text.x = element_text(size = 12, family = "Palatino")
      ) +
      labs(x = "", y = " ", title = "Impact matrix" ) +
      coord_cartesian(ylim = c(1, 11))
   
   
#  library(cowplot)
#  pimage <- axis_canvas(figure, axis = 'y') + 
#     draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Iris_virginica.jpg/295px-Iris_virginica.jpg", y = 1, scale = 0.5) +
#     draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Iris_virginica.jpg/295px-Iris_virginica.jpg", y = 2, scale = 0.5) +
#     draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Iris_versicolor_3.jpg/320px-Iris_versicolor_3.jpg", y = 3, scale = 0.5) +
#     draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Iris_virginica.jpg/295px-Iris_virginica.jpg", y = 4, scale = 0.5) +
#     draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Iris_virginica.jpg/295px-Iris_virginica.jpg", y = 5, scale = 0.5) +
#     draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Iris_versicolor_3.jpg/320px-Iris_versicolor_3.jpg", y = 6, scale = 0.5) +
#     draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Iris_virginica.jpg/295px-Iris_virginica.jpg", y = 7, scale = 0.5) +
#     draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Iris_versicolor_3.jpg/320px-Iris_versicolor_3.jpg", y = 8, scale = 0.5) +
#     draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Kosaciec_szczecinkowaty_Iris_setosa.jpg/450px-Kosaciec_szczecinkowaty_Iris_setosa.jpg", y = 9, scale = 0.5)
   
   
   # insert the image strip into the plot
   #ggdraw(insert_yaxis_grob(figure, pimage, position = "left"))
   
   
   #matrix = figure
   matrix = ggplotly(figure, height=800, width=1500, tooltip=c('text')) # "Name" or "University"
   
     
   
   
   return(matrix)
}