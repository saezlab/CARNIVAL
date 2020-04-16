## This code writes the boundaries of each variable.
##
## Enio Gjerga, 2020

write_boundaries <- function(variables=variables, oF=oF){
  
  M <- 100
  
  bounds <- c()
  
  for(i in seq_len(length(variables))){
    
    bounds <- c(bounds, 
                paste0("\t", 
                       "-1 <= ", 
                       variables[[i]]$variables[variables[[i]]$idxNodes], 
                       " <= 1"))
    bounds <- c(bounds, 
                paste0("\t", 
                       "0 <= ", 
                       variables[[i]]$variables[variables[[i]]$idxNodesUp], 
                       " <= 1"))
    bounds <- c(bounds, 
                paste0("\t", 
                       "0 <= ", 
                       variables[[i]]$variables[variables[[i]]$idxNodesDown], 
                       " <= 1"))
    bounds <- c(bounds, 
                paste0("\t", 
                       "0 <= ", 
                       variables[[i]]$variables[variables[[i]]$idxEdgesUp], 
                       " <= 1"))
    bounds <- c(bounds, 
                paste0("\t", 
                       "0 <= ",  
                       variables[[i]]$variables[variables[[i]]$idxEdgesDown], 
                       " <= 1"))
    bounds <- c(bounds, paste0("\t", 
                               "-1 <= ", 
                               variables[[i]]$variables[variables[[i]]$idxB], 
                               " <= 1"))
    bounds <- c(bounds, paste0("\t", 
                               "0 <= ", 
                               variables[[i]]$variables[variables[[i]]$idxDist], 
                               " <= ", M))
    bounds <- c(bounds, paste0("\t", 
                               "0 <= ", 
                               unique(strsplit(oF, split = " ")[[1]][grep(
                                 pattern = "absDiff", 
                                 x = strsplit(oF, split = " ")[[1]])]), 
                               " <= 2"))
    bounds <- c(bounds, paste0("\t", "0 <= ", 
                               variables[[i]]$variables[variables[[i]]$idxDist], 
                               " <= ", M))
    
  }
  
  return(bounds)
  
}