write_constraints_2_all <- function(variables=variables){
  
  constraints2 <- c()
  
  for(i in 1:length(variables)){
    
    var <- variables[[i]]
    
    constraints2 <- c(constraints2, write_constraints_2(variables = var, conditionIDX = i))
    
  }
  
  return(constraints2)
  
}
