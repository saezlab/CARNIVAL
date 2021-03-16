##  This code writes the list of constraints (6) of the ILP problem for one condition.
##
## Enio Gjerga, 2020

write_constraints_6 <- function(variables = variables,
                                dataMatrix = dataMatrix,
                                priorKnowledgeNetwork = priorKnowledgeNetwork) {

  ii=1
  
  source <- unique(variables[[ii]]$reactionSource)
  target <- unique(variables[[ii]]$reactionTarget)
  
  gg <- igraph::graph_from_data_frame(d = priorKnowledgeNetwork[, c(3, 1)])
  adj <- igraph::get.adjacency(gg)
  adj <- as.matrix(adj)
  
  idx1 <- which(rowSums(adj)==0)
  idx2 <- setdiff(seq_len(nrow(adj)), idx1)
  
  cc1 = NULL
  if(length(idx1)>0){
    
    cc1 <-
      paste0(variables[[ii]]$variables[which(
          variables[[ii]]$exp%in%paste0(
            "SpeciesUP ",
            rownames(adj)[idx1],
            " in experiment ", ii))], " <= 0")
    
  } else {
    cc1 <- NULL
  }
  
  cc2 <- rep("", length(idx2))
  
  for(i in seq_len(length(idx2))){
    
    cc2[i] <- paste0(
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
    
  }

  return(c(cc1, cc2))
}
