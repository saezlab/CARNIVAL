## This function writes the constraints preventing self-activation of nodes in 
## the network due to positive feedback loops.
## 
## Enio Gjerga, 2020

write_loop_constraints <- function(variables=variables, 
                                   pknList=pknList, 
                                   inputs=inputs) {
  
  M <- 101
  constraints1 <- c()
  constraints2 <- c()
  constraints3 <- c()
  constraints4 <- c()
  
  if(length(which(pknList[, 3]%in%colnames(inputs)))==0){
    
    pkn <- pknList
    
  }
  
  if(length(which(pknList[, 3]%in%colnames(inputs))) > 0){
    
    pkn <- pknList[-which(pknList[, 3]%in%colnames(inputs)), ]
    
  }
  
  for(ii in 1:length(variables)){
    
    reactionsUp <- 
      variables[[ii]]$variables[which(
        variables[[ii]]$exp%in%paste0(
          "ReactionUp ", 
          pkn[, 1], "=", pkn[, 3], " in experiment ", ii))]
    reactionsDown <- 
      variables[[ii]]$variables[which(
        variables[[ii]]$exp%in%paste0(
          "ReactionDown ", pkn[, 1], "=", pkn[, 3], " in experiment ", ii))]
    
    ##
    if(length(which(pknList[, 3]%in%colnames(inputs))) > 0){
      kk <- sapply(
        strsplit(
          variables[[ii]]$exp[variables[[ii]]$idxEdgesUp], 
          split = " "), function(x) x[2])[-which(
            pknList[, 3]%in%colnames(inputs))]
    }
    if(length(which(pknList[, 3]%in%colnames(inputs))) == 0){
      kk <- sapply(
        strsplit(
          variables[[ii]]$exp[variables[[ii]]$idxEdgesUp], split = " "), 
        function(x) x[2])
    }
    cc <- paste0(M, " ", reactionsUp, " + dist_", 
                 sapply(strsplit(kk, split = "="), function(x) x[1]), 
                 " - dist_", sapply(strsplit(kk, split = "="), 
                                    function(x) x[2]), " <= ", M-1)
    constraints1 <- c(constraints1, cc)
    
    ##
    if(length(which(pknList[, 3]%in%colnames(inputs))) > 0){
      kk <- sapply(
        strsplit(
          variables[[ii]]$exp[variables[[ii]]$idxEdgesDown], 
          split = " "), 
        function(x) x[2])[-which(pknList[, 3]%in%colnames(inputs))]
    }
    if(length(which(pknList[, 3]%in%colnames(inputs))) == 0){
      kk <- sapply(
        strsplit(
          variables[[ii]]$exp[variables[[ii]]$idxEdgesDown], 
          split = " "), function(x) x[2])
    }
    cc <- paste0(M, " ", reactionsDown, " + dist_", 
                 sapply(strsplit(kk, split = "="), 
                        function(x) x[1]), " - dist_", 
                 sapply(strsplit(kk, split = "="), 
                        function(x) x[2]), " <= ", M-1)
    constraints2 <- c(constraints2, cc)
    
  }
  
  return(c(constraints1, constraints2))
  
}
