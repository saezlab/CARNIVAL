#'\code{write_loop_constraints}
#'
#'@param variables Contains the list of variables as used to formulate the ILP problem, explanations for each variable and a list of useful indices.
#'@param pknList Contains the background network which serves as a prior knowledge and which we train.
#'@param inputs Contains the list of targets as inputs.
#'
#'@return This function writes the constraints preventing self-activation of nodes in the network due to positive feedback loops.

write_loop_constraints <- function(variables=variables, pknList=pknList, inputs=inputs) {
  
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
    
    reactionsUp <- variables[[ii]]$variables[which(variables[[ii]]$exp%in%paste0("ReactionUp ", pkn[, 1], "=", pkn[, 3], " in experiment ", ii))]
    reactionsDown <- variables[[ii]]$variables[which(variables[[ii]]$exp%in%paste0("ReactionDown ", pkn[, 1], "=", pkn[, 3], " in experiment ", ii))]
    
    ##
    if(length(which(pknList[, 3]%in%colnames(inputs))) > 0){
      kk <- sapply(strsplit(variables[[ii]]$exp[variables[[ii]]$idxEdgesUp], split = " "), function(x) x[2])[-which(pknList[, 3]%in%colnames(inputs))]
    }
    if(length(which(pknList[, 3]%in%colnames(inputs))) == 0){
      kk <- sapply(strsplit(variables[[ii]]$exp[variables[[ii]]$idxEdgesUp], split = " "), function(x) x[2])
    }
    cc <- paste0(M, " ", reactionsUp, " + dist_", sapply(strsplit(kk, split = "="), function(x) x[1]), " - dist_", sapply(strsplit(kk, split = "="), function(x) x[2]), " <= ", M-1)
    constraints1 <- c(constraints1, cc)
    
    ##
    if(length(which(pknList[, 3]%in%colnames(inputs))) > 0){
      kk <- sapply(strsplit(variables[[ii]]$exp[variables[[ii]]$idxEdgesDown], split = " "), function(x) x[2])[-which(pknList[, 3]%in%colnames(inputs))]
    }
    if(length(which(pknList[, 3]%in%colnames(inputs))) == 0){
      kk <- sapply(strsplit(variables[[ii]]$exp[variables[[ii]]$idxEdgesDown], split = " "), function(x) x[2])
    }
    cc <- paste0(M, " ", reactionsDown, " + dist_", sapply(strsplit(kk, split = "="), function(x) x[1]), " - dist_", sapply(strsplit(kk, split = "="), function(x) x[2]), " <= ", M-1)
    constraints2 <- c(constraints2, cc)
    
    # ##
    # species <-  unique(c(pknList[, 1], pknList[, 3]))
    # 
    # cc <- paste0("dist_", species, " >= 0")
    # constraints3 <- c(constraints3, cc)
    # 
    # cc <- paste0("dist_", species, " <= ", M-1)
    # constraints4 <- c(constraints4, cc)
    
  }
  
  # return(c(constraints1, constraints2, constraints3, constraints4))
  return(c(constraints1, constraints2))
  
}
