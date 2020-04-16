## This code writes all the variables.
##
## Enio Gjerga, 2020

write_generals <- function(variables=variables, oF=oF){
  
  generals <- c()
  
  for(i in seq_len(length(variables))){
    
    generals <- c(generals, 
                  paste0("\t", 
                         variables[[i]]$variables[variables[[i]]$idxNodes]))
    generals <- c(generals, 
                  paste0("\t", 
                         variables[[i]]$variables[variables[[i]]$idxB]))
    generals <- c(generals, 
                  paste0("\t",
                         unique(strsplit(oF, split = " ")[[1]][grep(
                           pattern = "absDiff", 
                           x = strsplit(oF, split = " ")[[1]])])))

  }
  
  return(generals)
  
}