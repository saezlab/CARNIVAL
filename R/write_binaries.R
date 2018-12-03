#'\code{write_binaries}
#'
#'@param variables Contains the list of variables as used to formulate the ILP problem, explanations for each variable and a list of useful indeces.
#'@return This code writes the list of binary variables (xp, xm, up & um).

write_binaries <- function(variables=variables){
  
  binaries <- c()
  
  for(i in 1:length(variables)){
    
    binaries <- c(binaries, paste0("\t", variables[[i]]$variables[variables[[i]]$idxNodesUp]))
    binaries <- c(binaries, paste0("\t", variables[[i]]$variables[variables[[i]]$idxNodesDown]))
    binaries <- c(binaries, paste0("\t", variables[[i]]$variables[variables[[i]]$idxEdgesUp]))
    binaries <- c(binaries, paste0("\t", variables[[i]]$variables[variables[[i]]$idxEdgesDown]))
    
  }
  
  return(binaries)
  
}