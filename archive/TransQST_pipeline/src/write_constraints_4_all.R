write_constraints_4_all <- function(variables=variables) {
  
  constraints4 <- c()
  
  for(i in 1:length(variables)){
    
    var <- variables[[i]]
    
    constraints4 <- c(constraints4, write_constraints_4(variables = var, conditionIDX = i))
    
  }
  
  return(constraints4)
  
}