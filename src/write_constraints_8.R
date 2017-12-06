##
write_constraints_8 <- function(variables=variables, inputs=inputs){
  
  constraints8 <- list()
  
  # inputs <- as.vector(t(inputs))
  
  inputsZero <- which(inputs==0, arr.ind = TRUE)
  inputsUp <- which(inputs==1, arr.ind = TRUE)
  inputsDown <- which(inputs==-1, arr.ind = TRUE)
  
  # ii <- as.vector(t(inputs))
  
  for(i in 1:length(variables)){
    
    constraints8[[i]] <- paste0(variables[[i]]$variables[variables[[i]]$idxNodesUp], " - ",
                                variables[[i]]$variables[variables[[i]]$idxNodesDown], " - ",
                                variables[[i]]$variables[variables[[i]]$idxNodes], " = 0")
    
  }
  
  if(nrow(inputsUp) > 0){
    
    for(ii in 1:length(variables)){
      
      for(i in 1:nrow(inputsUp)){
        
        ss1 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesUP ", colnames(inputs)[inputsUp[i, 2]], " in experiment ", inputsUp[i, 1]))]
        
        ss2 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesDown ", colnames(inputs)[inputsUp[i, 2]], " in experiment ", inputsUp[i, 1]))]
        
        ss3 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("Species ", colnames(inputs)[inputsUp[i, 2]], " in experiment ", inputsUp[i, 1]))]
        
        idx <- which(constraints8[[ii]]==paste0(ss1, " - ", ss2, " - ", ss3, " = 0"))
        
        constraints8[[ii]][idx] <- gsub(constraints8[[ii]][idx], pattern = " = 0", replacement = " = -1")
        
      }
      
    }
    
  }
  
  if(nrow(inputsDown) > 0){
    
    for(ii in 1:length(variables)){
      
      for(i in 1:nrow(inputsDown)){
        
        ss1 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesUP ", colnames(inputs)[inputsDown[i, 2]], " in experiment ", inputsDown[i, 1]))]
        
        ss2 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesDown ", colnames(inputs)[inputsDown[i, 2]], " in experiment ", inputsDown[i, 1]))]
        
        ss3 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("Species ", colnames(inputs)[inputsDown[i, 2]], " in experiment ", inputsDown[i, 1]))]
        
        idx <- which(constraints8[[ii]]==paste0(ss1, " - ", ss2, " - ", ss3, " = 0"))
        
        constraints8[[ii]][idx] <- gsub(constraints8[[ii]][idx], pattern = " = 0", replacement = " = 1")
        
      }
      
    }
    
  }
  
  if(nrow(inputsZero) > 0){
    
    for(i in 1:nrow(inputsZero)){
      
      ss <- variables[[inputsZero[i, 1]]]$variables[which(variables[[inputsZero[i, 1]]]$exp==paste0("Species ", colnames(inputs)[inputsZero[i, 2]], " in experiment ", inputsZero[i, 1]))]
      
      constraints8[[inputsZero[i, 1]]] <- c(constraints8[[inputsZero[i, 1]]], paste0(ss, " = 0"))
      
    }
    
  }
  
  if(nrow(inputsUp) > 0){
    
    for(i in 1:nrow(inputsUp)){
      
      ss <- variables[[inputsUp[i, 1]]]$variables[which(variables[[inputsUp[i, 1]]]$exp==paste0("Species ", colnames(inputs)[inputsUp[i, 2]], " in experiment ", inputsUp[i, 1]))]
      
      constraints8[[inputsUp[i, 1]]] <- c(constraints8[[inputsUp[i, 1]]], paste0(ss, " = 1"))
      
    }
    
  }
  
  if(nrow(inputsDown) > 0){
    
    for(i in 1:nrow(inputsDown)){
      
      ss <- variables[[inputsDown[i, 1]]]$variables[which(variables[[inputsDown[i, 1]]]$exp==paste0("Species ", colnames(inputs)[inputsDown[i, 2]], " in experiment ", inputsDown[i, 1]))]
      
      constraints8[[inputsDown[i, 1]]] <- c(constraints8[[inputsDown[i, 1]]], paste0(ss, " = -1"))
      
    }
    
  }
  
  print(constraints8)
  
  return(unlist(constraints8))
  
}
