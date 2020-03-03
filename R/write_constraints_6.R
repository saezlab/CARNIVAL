#'\code{write_constraints_6}
#'
#' This code writes the list of constraints (6) of the ILP problem for all the 
#' conditions.
#' 
#' Enio Gjerga, 2020

write_constraints_6 <- function(variables=variables, 
                                dataMatrix=dataMatrix, 
                                inputs = inputs) {
  
  library(igraph)
  constraints6 <- c()
  
  for(ii in 1:length(variables)){
    
    source <- unique(variables[[ii]]$reactionSource)
    target <- unique(variables[[ii]]$reactionTarget)
    
    gg <- graph_from_data_frame(d = pknList[, c(3, 1)])
    adj <- get.adjacency(gg)
    adj <- as.matrix(adj)
    
    idx1 <- which(rowSums(adj)==0)
    idx2 <- setdiff(1:nrow(adj), idx1)
    
    if(length(idx1)>0){
      
      constraints6 <- 
        c(constraints6, 
          paste0(variables[[ii]]$variables[which(
            variables[[ii]]$exp%in%paste0(
              "SpeciesUP ", 
              rownames(adj)[idx1], 
              " in experiment ", ii))], " <= 0"))
      
    }
    
    for(i in 1:length(idx2)){
      
      cc <- paste0(
        variables[[ii]]$variables[which(
          variables[[ii]]$exp==paste0(
            "SpeciesUP ", 
            rownames(adj)[idx2[i]], 
            " in experiment ", ii))], 
                   paste(
                     paste0(
                       " - ", 
                       variables[[ii]]$variables[which(
                         variables[[ii]]$exp%in%paste0(
                           "ReactionUp ", 
                           colnames(adj)[which(adj[idx2[i], ]>0)], 
                           "=", 
                           rownames(adj)[idx2[i]], 
                           " in experiment ", ii))]), collapse = ""), " <= 0")
      
      constraints6 <- c(constraints6, cc)
      
    }
    
  }
  
  return(constraints6)
}
