write_constraints_3_all <- function(variables=variables) {
  
  constraints3 <- c()
  
  for(i in 1:length(variables)){
    
    var <- variables[[i]]
    
    constraints3 <- c(constraints3, write_constraints_3(variables = var))
    
  }
  
  return(constraints3)
  
}