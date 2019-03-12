write_experimental_conditions_constraints <- function(variables = variables){
  
  constraints = c()
  
  constraints1 = c()
  for(ii in 1:(length(variables)-1)){
    
    c1 = paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges], " - ", variables[[ii]]$variables[variables[[ii]]$idxEdgesUp], " - ", variables[[ii]]$variables[variables[[ii]]$idxEdgesDown], " = 0")
    constraints1 <- c(constraints1, c1)
    
  }
  
  constraints2 = c()
  for(ii in 1:length(variables$`Reaction Variables`$Variables)){
    
    for(jj in 1:(length(variables)-1)){
      
      c2 = variables$`Reaction Variables`$Variables[ii]
      c2 <- paste0(c2, " - ", variables[[jj]]$variables[variables[[jj]]$idxEdges[ii]])
      
      constraints2 = c(constraints2, paste0(c2, " = 0"))
      
    }
    
  }
  
  constraints = c(constraints1, constraints2)
  
  return(constraints)
  
}