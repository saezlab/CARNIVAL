##
write_constraints_8 <- function(variables=variables, inputs=inputs){
  
  constraints8 <- c()
  
  for(ii in 1:length(variables)){
    
    ##
    cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxNodesUp], " - ", variables[[ii]]$variables[variables[[ii]]$idxNodesDown], 
                 " + ", variables[[ii]]$variables[variables[[ii]]$idxB], " - ", variables[[ii]]$variables[variables[[ii]]$idxNodes], " = 0")
    
    constraints8 <- c(constraints8, cc)
    
    ##
    kk <- paste0("Species ", colnames(inputs), " in experiment ", ii)
    cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxB[which(!(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk))]], " = 0")
    constraints8 <- c(constraints8, cc)
    
    ##
    kk <- paste0("Species ", colnames(inputs), " in experiment ", ii)
    cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxNodes[which(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk)]], " = ", inputs[ii, ])
    constraints8 <- c(constraints8, cc)
    
    ##
    kk <- paste0("Species ", colnames(inputs), " in experiment ", ii)
    cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxNodes[which(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk)]], " - ", 
                 variables[[ii]]$variables[variables[[ii]]$idxB[which(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk)]], " = 0")
    constraints8 <- c(constraints8, cc)
    
  }
  
  return(constraints8)
  
}
