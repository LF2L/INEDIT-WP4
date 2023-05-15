# Funcion para color la description of the case study
goals <- function(Test, ...) {
   
x <- Test$Results
for (i in x){
   cat("- ", i, "\n")
}

#return(x)
}