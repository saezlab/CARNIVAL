write_boundaries <- function(variables=variables){
  
  bounds <- c()
  
  for(i in 1:length(variables)){
    
    bounds <- c(bounds, paste0("\t", "-1 <= ", variables[[i]]$variables[variables[[i]]$idxNodes], " <= 1"))
    bounds <- c(bounds, paste0("\t", "0 <= ", variables[[i]]$variables[variables[[i]]$idxNodesUp], " <= 1"))
    bounds <- c(bounds, paste0("\t", "0 <= ", variables[[i]]$variables[variables[[i]]$idxNodesDown], " <= 1"))
    bounds <- c(bounds, paste0("\t", "0 <= ", variables[[i]]$variables[variables[[i]]$idxEdgesUp], " <= 1"))
    bounds <- c(bounds, paste0("\t", "0 <= ", variables[[i]]$variables[variables[[i]]$idxEdgesDown], " <= 1"))
    
  }
  
  return(bounds)
  
}