## This function writes the constraints preventing self-activation of nodes in 
## the network due to positive feedback loops.
## 
## Enio Gjerga, 2020

write_loop_constraints <- function(variables = variables, 
                                   perturbations = perturbations,
                                   priorKnowledgeNetwork = priorKnowledgeNetwork) {
  
  #TODO magic number? 
  M <- 101
  constraints1 <- c()
  constraints2 <- c()
  constraints3 <- c()
  constraints4 <- c()
  
  if(length(which(priorKnowledgeNetwork[, 3]%in%colnames(perturbations)))==0){
    
    pkn <- priorKnowledgeNetwork
    
  }
  
  if(length(which(priorKnowledgeNetwork[, 3]%in%colnames(perturbations))) > 0){
    
    pkn <- priorKnowledgeNetwork[-which(priorKnowledgeNetwork[, 3]%in%colnames(perturbations)), ]
    
  }
  
  for(ii in seq_len(length(variables))){
    
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
    if(length(which(priorKnowledgeNetwork[, 3]%in%colnames(perturbations))) > 0){
      kk <- sapply(
        strsplit(
          variables[[ii]]$exp[variables[[ii]]$idxEdgesUp], 
          split = " "), function(x) x[2])[-which(
            priorKnowledgeNetwork[, 3]%in%colnames(perturbations))]
    }
    if(length(which(priorKnowledgeNetwork[, 3]%in%colnames(perturbations))) == 0){
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
    if(length(which(priorKnowledgeNetwork[, 3]%in%colnames(perturbations))) > 0){
      kk <- sapply(
        strsplit(
          variables[[ii]]$exp[variables[[ii]]$idxEdgesDown], 
          split = " "), 
        function(x) x[2])[-which(priorKnowledgeNetwork[, 3]%in%colnames(perturbations))]
    }
    if(length(which(priorKnowledgeNetwork[, 3]%in%colnames(perturbations))) == 0){
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
