## This code writes the boundaries of each variable.
##
## Enio Gjerga, 2020

write_boundaries <- function(variables = variables, objectiveFunction = objectiveFunction){
  
  M <- 100
  
  i = 1
  
  cc1 <- paste0("\t", 
                     "-1 <= ", 
                     variables[[i]]$variables[variables[[i]]$idxNodes], 
                     " <= 1")
  cc2 <- paste0("\t", 
                     "0 <= ", 
                     variables[[i]]$variables[variables[[i]]$idxNodesUp], 
                     " <= 1")
  cc3 <- paste0("\t", 
                     "0 <= ", 
                     variables[[i]]$variables[variables[[i]]$idxNodesDown], 
                     " <= 1")
  cc4 <- paste0("\t", 
                     "0 <= ", 
                     variables[[i]]$variables[variables[[i]]$idxEdgesUp], 
                     " <= 1")
  cc5 <- paste0("\t", 
                     "0 <= ",  
                     variables[[i]]$variables[variables[[i]]$idxEdgesDown], 
                     " <= 1")
  cc6 <- paste0("\t", "-1 <= ", variables[[i]]$variables[variables[[i]]$idxB], 
                             " <= 1")
  cc7 <- paste0("\t", "0 <= ", variables[[i]]$variables[variables[[i]]$idxDist], 
                             " <= ", M)
  cc8 <- paste0("\t", "0 <= ", unique(strsplit(objectiveFunction, split = " ")[[1]][grep(
                               pattern = "absDiff", 
                               x = strsplit(objectiveFunction, split = " ")[[1]])]), 
                             " <= 2")
  cc9 <- paste0("\t", "0 <= ", variables[[i]]$variables[variables[[i]]$idxDist], 
                             " <= ", M)
  
  return(c(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9))
  
}