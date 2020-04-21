## This code writes the list of binary variables (xp, xm, up & um).
## 
## Enio Gjerga, 2020

write_binaries <- function(variables=variables){
  
  # binaries <- c()
  # 
  # for(i in seq_len(length(variables))){
  #   
  #   binaries <- c(binaries, 
  #                 paste0("\t", 
  #                        variables[[i]]$variables[variables[[i]]$idxNodesUp]))
  #   binaries <- c(binaries, 
  #                 paste0("\t", 
  #                        variables[[i]]$variables[variables[[i]]$idxNodesDown]))
  #   binaries <- c(binaries, 
  #                 paste0("\t", 
  #                        variables[[i]]$variables[variables[[i]]$idxEdgesUp]))
  #   binaries <- c(binaries, 
  #                 paste0("\t", 
  #                        variables[[i]]$variables[variables[[i]]$idxEdgesDown]))
  #   
  # }
  
  i = 1
  
  cc1 <- paste0("\t", 
                     variables[[i]]$variables[variables[[i]]$idxNodesUp])
  cc2 <- paste0("\t", 
                     variables[[i]]$variables[variables[[i]]$idxNodesDown])
  cc3 <- paste0("\t", 
                     variables[[i]]$variables[variables[[i]]$idxEdgesUp])
  cc4 <- paste0("\t", 
                     variables[[i]]$variables[variables[[i]]$idxEdgesDown])
  
  return(c(cc1, cc2, cc3, cc4))
  
}