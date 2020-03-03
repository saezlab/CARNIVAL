## Appending the binaries for each condition
##
## Enio Gjerga, 2020

append_binaries = function(binaries = binaries, variables = variables){
  
  for(ii in 1:length(variables$`Reaction Variables`$Explanation)){
    
    ss <- strsplit(x = 
                     strsplit(
                       x = variables$`Reaction Variables`$Explanation[ii], 
                       split = " ", fixed = TRUE)[[1]][2], 
                   split = "=", fixed = TRUE)[[1]][1]
    tt <- strsplit(x = 
                     strsplit(
                       x = variables$`Reaction Variables`$Explanation[ii], 
                       split = " ", fixed = TRUE)[[1]][2], 
                   split = "=", fixed = TRUE)[[1]][2]
    
    for(jj in 1:(length(variables)-1)){
      
      binaries <- c(binaries, paste0("\tandP_", ss, "_", tt, "_", jj))
      binaries <- c(binaries, paste0("\tandM_", ss, "_", tt, "_", jj))
      
    }
    
  }
  
  return(binaries)
  
}