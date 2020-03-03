#'\code{write_binaries}
#'
#' This code writes the list of binary variables (xp, xm, up & um).
#' 
#' Enio Gjerga, 2020

write_binaries <- function(variables=variables){
  
  binaries <- c()
  
  for(i in 1:length(variables)){
    
    binaries <- c(binaries, 
                  paste0("\t", 
                         variables[[i]]$variables[variables[[i]]$idxNodesUp]))
    binaries <- c(binaries, 
                  paste0("\t", 
                         variables[[i]]$variables[variables[[i]]$idxNodesDown]))
    binaries <- c(binaries, 
                  paste0("\t", 
                         variables[[i]]$variables[variables[[i]]$idxEdgesUp]))
    binaries <- c(binaries, 
                  paste0("\t", 
                         variables[[i]]$variables[variables[[i]]$idxEdgesDown]))
    
  }
  
  return(binaries)
  
}