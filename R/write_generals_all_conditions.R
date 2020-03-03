#'\code{write_generals_all_conditions}
#'
#'@param variables Contains the list of variables as used to formulate the ILP 
#'problem, explanations for each variable and a list of useful indices.
#'@param oF Is the objective function of the formulation.
#'
#'@return This code writes all the variables.
#'
#'Enio Gjerga, 2020

write_generals_all_conditions <- function(variables=variables, oF=oF){
  
  generals <- c()
  
  for(i in 1:length(variables)){
    
    if(i != length(variables)){
      
      generals <- 
        c(generals, 
          paste0("\t", variables[[i]]$variables[variables[[i]]$idxNodes]))
      generals <- 
        c(generals, 
          paste0("\t", variables[[i]]$variables[variables[[i]]$idxB]))
      generals <- 
        c(generals, 
          paste0("\t", 
                 unique(
                   strsplit(oF, 
                            split = " ")[[1]][grep(
                              pattern = "absDiff", 
                              x = strsplit(oF, split = " ")[[1]])])))
      
    }
    
  }
  
  return(generals)
  
}