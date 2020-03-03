#'\code{write_binaries_all_conditions}
#'
#'@param variables Contains the list of variables as used to formulate the ILP 
#'problem, explanations for each variable and a list of useful indices.
#'
#'@return This code writes the list of binary variables (xp, xm, up & um).
#'
#'Enio Gjerga, 2020

write_binaries_all_conditions <- function(variables=variables){
  
  binaries <- c()
  
  for(i in 1:length(variables)){
    
    if(i != length(variables)){
      
      binaries <- 
        c(binaries, 
          paste0("\t", variables[[i]]$variables[variables[[i]]$idxNodesUp]))
      binaries <- 
        c(binaries, 
          paste0("\t", variables[[i]]$variables[variables[[i]]$idxNodesDown]))
      binaries <- 
        c(binaries, 
          paste0("\t", variables[[i]]$variables[variables[[i]]$idxEdgesUp]))
      binaries <- 
        c(binaries, 
          paste0("\t", variables[[i]]$variables[variables[[i]]$idxEdgesDown]))
      binaries <- 
        c(binaries, 
          paste0("\t", variables[[i]]$variables[variables[[i]]$idxEdges]))
      
    } else {
      
      binaries <- c(binaries, paste0("\t", variables[[i]]$Variables))
      
    }
    
  }
  
  return(binaries)
  
}