write_constraints_7 <- function(variables=variables, dataMatrix=dataMatrix, inputs=inputs) {
  
  constraints7 <- c()
  
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    source <- unique(variables[[i]]$reactionSource)
    target <- unique(variables[[i]]$reactionTarget)
    
    for(ii in 1:length(target)){
      
      pp1 <- variables[[i]]$variables[which(variables[[i]]$exp==paste0("SpeciesDown ", target[ii], " in experiment ", i))]
      pp2 <- ""
      
      for(jj in 1:length(source)){
        
        reaction <- paste0("ReactionDown ", source[jj], "=", target[ii], " in experiment ", i)
        
        if(length(which(variables[[i]]$exp==reaction)) > 0){
          
          for(kk in 1:length(which(variables[[i]]$exp==reaction))){
            
            pp2 <- paste0(pp2, " - ", variables[[i]]$variables[which(variables[[i]]$exp==reaction)[kk]])
            
          }
          
        }
        
      }
      
      constraints7 <- c(constraints7, paste0(pp1, pp2, " <= 0"))
      
    }
    
    for(j in 1:ncol(inputs)){
      
      cc <- paste0(variables[[i]]$variables[which(variables[[i]]$exp==paste0("SpeciesDown ", colnames(inputs)[j], " in experiment ", i))], " <= 0")
      
      constraints7 <- c(constraints7, cc)
      
    }
    
  }
  
  return(constraints7)
  
}