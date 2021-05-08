## Obsolete code kept here for backward compatibility for v.2.1. 
# Planned to be removed in v.3
createConstraintsObjectiveFunction <- function(variables = variables, 
                                               dataVector = dataVector, 
                                               postfix="_1"){
  .Deprecated("createConstraintsMeasurements_v2")
  measurements <- dataVector$dataVectorSign
  
  idx2 <- which(measurements == 1)
  idx3 <- which(measurements == -1)
  
  cc1 <- rep("", length(measurements))
  cc2 <- rep("", length(measurements))
  
  cc1[idx2] <- paste0(variables$variables[idx2], " - absDiff", 
                      idx2, postfix, " <= 1")
  cc2[idx2] <- paste0(variables$variables[idx2], " + absDiff", idx2, postfix, " >= 1")
  
  cc1[idx3] <- paste0(variables$variables[idx3], " - absDiff", idx3, postfix, " <= -1")
  cc2[idx3] <- paste0(variables$variables[idx3], " + absDiff", idx3, postfix, " >= -1")
  
  constraints0 <- c(cc1, cc2)
  
  return(constraints0[-which(constraints0=="")])
  
} 

createConstraints_1 <- function(variables = variables){
  .Deprecated("createConstraints_1_2_v2")
  selectEdges <- function(idx, reactionType) {
    gsub(variables$exp[idx], 
         pattern = reactionType, 
         replacement = "")
  }
  
  findMatchingVariable <- function(selectedEdges) {
    nodesFromEdge <- unlist(strsplit(selectedEdges, split = "="))
    varsToFind <- paste0("Species ", nodesFromEdge)[c(TRUE, FALSE)]
    vars[match(varsToFind, variables$exp)]
  }
  
  vars <- variables$variables 
  
  constraints1 <- rep("", length(variables$idxEdgesUp))
  # idx1 corresponds to activations, idx2 to inhibition
  idx1 <- which(variables$signs == 1)
  idx2 <- which(variables$signs == -1) 
  
  idxEdgesUp1 <- variables$idxEdgesUp[idx1]
  idxEdgesUp2 <- variables$idxEdgesUp[idx2]
  
  selectedEdges1 <- selectEdges(idxEdgesUp1, "ReactionUp ")
  selectedEdges2 <- selectEdges(idxEdgesUp2, "ReactionUp ")
  
  constraints1[idx1] <- createConstraint( vars[idxEdgesUp1], 
                                          "-",
                                          findMatchingVariable(selectedEdges1), 
                                          ">=", 0 )
  
  constraints1[idx2] <- createConstraint( vars[idxEdgesUp2], 
                                          "+",
                                          findMatchingVariable(selectedEdges2), 
                                          ">=", 0 )
  
  return(constraints1)
  
}

createConstraints_2 <- function(variables = variables){
  .Deprecated("createConstraints_1_2_v2")
  vars <- variables$variables
  
  selectEdges <- function(idx, reactionType) {
    gsub(variables$exp[idx], 
         pattern = reactionType, 
         replacement = "")
  }
  
  findMatchingVariable <- function(selectedEdges) {
    nodesFromEdge <- unlist(strsplit(selectedEdges, split = "="))
    varsToFind <- paste0("Species ", nodesFromEdge)[c(TRUE, FALSE)]
    vars[match(varsToFind, variables$exp)]
  }
  
  constraints1 <- rep("", length(variables$idxEdgesUp))
  # idx1 corresponds to activations, idx2 to inhibition
  idx1 <- which(variables$signs == 1)
  idx2 <- which(variables$signs == -1) 
  
  idxEdgesDown1 <- variables$idxEdgesDown[idx1]
  idxEdgesDown2 <- variables$idxEdgesDown[idx2]
  
  selectedEdges1 <- selectEdges(idxEdgesDown1, "ReactionDown ")
  selectedEdges2 <- selectEdges(idxEdgesDown2, "ReactionDown ")
  
  constraints1[idx1] <- createConstraint( vars[idxEdgesDown1], 
                                          "+",
                                          findMatchingVariable(selectedEdges1), 
                                          ">=", 0)
  
  constraints1[idx2] <- createConstraint( vars[idxEdgesDown2], 
                                          "-",
                                          findMatchingVariable(selectedEdges2), 
                                          ">=", 0)
  
  return(constraints1)
  
}

createConstraints_3 <- function(variables = variables) {
  .Deprecated("createConstraints_3_v2")  
  constraints3 <- paste0(
    variables$variables[variables$idxEdgesUp], 
    " + ", 
    variables$variables[variables$idxEdgesDown], 
    " <= 1")
  
  return(constraints3)
  
}

createConstraints_4 <- function(variables=variables) {
  .Deprecated("createConstraints_4_5_v2")
  vars <- variables$variables
  constraints4 <- rep("", length(variables$idxEdgesUp))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints4[idx1] <- paste0(
    vars[variables$idxEdgesUp[idx1]], 
    " - ",
    vars[match(
      paste0(
        "Species ",
        unlist(
          strsplit(
            gsub(
              variables$exp[variables$idxEdgesUp[idx1]], 
              pattern = "ReactionUp ", 
              replacement = ""), 
            split = "="))[c(TRUE, FALSE)]), 
      variables$exp)], 
    " - ",
    variables$uTable[match(
      vars[variables$idxEdgesUp[idx1]], 
      variables$uTable[, 1]), 2], " <= 0")
  
  constraints4[idx2] <- paste0(
    vars[variables$idxEdgesUp[idx2]], 
    " + ",
    vars[match(
      paste0(
        "Species ",
        unlist(
          strsplit(
            gsub(
              variables$exp[variables$idxEdgesUp[idx2]], 
              pattern = "ReactionUp ", 
              replacement = ""),
            split = "="))[c(TRUE, FALSE)]), 
      variables$exp)], 
    " - ",
    variables$uTable[match(
      vars[variables$idxEdgesUp[idx2]], 
      variables$uTable[, 1]), 2], " <= 0")
  
  return(constraints4)
  
}

createConstraints_5 <- function(variables=variables) {
  .Deprecated("createConstraints_4_5_v2")
  vars <- variables$variables
  constraints1 <- rep("", length(variables$idxEdgesDown))
  
  idx1 <- which(variables$signs == 1)
  idx2 <- which(variables$signs == -1)
  
  constraints1[idx1] <- paste0(
    vars[variables$idxEdgesDown[idx1]], 
    " + ",
    vars[match(
      paste0(
        "Species ",
        unlist(
          strsplit(
            gsub(
              variables$exp[variables$idxEdgesDown[idx1]], 
              pattern = "ReactionDown ", 
              replacement = ""),  
            split = "="))[c(TRUE, FALSE)]), variables$exp)], 
    " - ",
    variables$uTable[match(
      vars[variables$idxEdgesDown[idx1]], 
      variables$uTable[, 2]), 1], " <= 0")
  
  constraints1[idx2] <- paste0(
    vars[variables$idxEdgesDown[idx2]], 
    " - ",
    vars[match(
      paste0(
        "Species ",
        unlist(
          strsplit(
            gsub(
              variables$exp[variables$idxEdgesDown[idx2]], 
              pattern = "ReactionDown ", 
              replacement = ""), 
            split = "="))[c(TRUE, FALSE)]), variables$exp)], 
    " - ",
    variables$uTable[match(
      vars[variables$idxEdgesDown[idx2]], 
      variables$uTable[, 2]), 1], " <= 0")
  
  return(constraints1)
  
}

createConstraints_6 <- function(variables = variables,
                                priorKnowledgeNetwork = priorKnowledgeNetwork) {
  .Deprecated("createConstraints_6_7_v2")
  vars <- variables$variables
  
  source <- unique(variables$reactionSource)
  target <- unique(variables$reactionTarget)
  
  gg <- igraph::graph_from_data_frame(d = priorKnowledgeNetwork[, c(3, 1)])
  adj <- igraph::get.adjacency(gg)
  adj <- as.matrix(adj)
  
  idx1 <- which(rowSums(adj) == 0)
  idx2 <- setdiff(seq_len(nrow(adj)), idx1)
  
  cc1 <- NULL
  
  if( length(idx1) > 0 ){
    
    cc1 <-
      paste0(vars[which(
        variables$exp %in% paste0(
          "SpeciesUP ",
          rownames(adj)[idx1]))], " <= 0")
    
  } else {
    cc1 <- NULL
  }
  
  cc2 <- rep("", length(idx2))
  
  for(i in seq_len(length(idx2))){
    
    cc2[i] <- paste0(
      vars[which(
        variables$exp == paste0(
          "SpeciesUP ",
          rownames(adj)[idx2[i]]))],
      paste(
        paste0(
          " - ",
          vars[which(
            variables$exp %in% paste0(
              "ReactionUp ",
              colnames(adj)[which(adj[idx2[i], ]>0)],
              "=",
              rownames(adj)[idx2[i]]))]), collapse = ""), " <= 0")
    
  }
  
  return(c(cc1, cc2))
}

createConstraints_7 <- function(variables = variables,
                                priorKnowledgeNetwork = priorKnowledgeNetwork) {
  .Deprecated("createConstraints_6_7_v2")
  vars <- variables$variables
  
  source <- unique(variables$reactionSource)
  target <- unique(variables$reactionTarget)
  
  gg <- igraph::graph_from_data_frame(d = priorKnowledgeNetwork[, c(3, 1)])
  adj <- igraph::get.adjacency(gg)
  adj <- as.matrix(adj)
  
  idx1 <- which(rowSums(adj)==0)
  idx2 <- setdiff(seq_len(nrow(adj)), idx1)
  cc1 <- rep("", length(idx1))
  
  if (length(idx1)>0) {
    cc1 <-
      paste0(
        vars[which(
          variables$exp %in% paste0(
            "SpeciesDown ",
            rownames(adj)[idx1]))], " <= 0")
  }
  
  cc2 <- rep("", length(idx2))
  for(i in seq_len(length(idx2))){
    
    cc2[i] <- paste0(
      vars[which(
        variables$exp==paste0(
          "SpeciesDown ",
          rownames(adj)[idx2[i]]))],
      paste(
        paste0(
          " - ",
          vars[which(
            variables$exp %in% paste0(
              "ReactionDown ",
              colnames(adj)[which(adj[idx2[i], ]>0)],
              "=", rownames(adj)[idx2[i]]))]), collapse = ""), " <= 0")
    
  }
  
  return(c(cc1, cc2))
}

createConstraints_8 <- function(variables = variables, 
                                perturbations = perturbations, 
                                priorKnowledgeNetwork = priorKnowledgeNetwork){
  .Deprecated("createConstraints_8_v2")
  vars <- variables$variables
  
  cc1 <- paste0(
    vars[variables$idxNodesUp], 
    " - ", 
    vars[variables$idxNodesDown], 
    " + ", vars[variables$idxB], 
    " - ", vars[variables$idxNodes], " = 0")
  
  kk <- paste0("Species ", names(perturbations))
  cc2 <- paste0(
    vars[variables$idxB[which(
      !(variables$exp[variables$idxNodes] %in% kk))]], " = 0")
  
  kk <- paste0("Species ", names(perturbations))
  cc3 <- c()
  for(jj in seq_len(length(kk))){
    cName <- strsplit(x = kk[jj], split = " ")[[1]][2]
    cc3 <- c(cc3, paste0(
      vars[which(
        variables$exp == paste0(
          "Species ", cName))], " = ", perturbations[jj]))
  }
  
  cc4 <- c()
  if (length(
    setdiff(
      as.character(priorKnowledgeNetwork[, 1]), 
      as.character(priorKnowledgeNetwork[, 3]))) > 0) {
    kk <- paste0(
      "Species ", 
      setdiff(as.character(priorKnowledgeNetwork[, 1]), 
              as.character(priorKnowledgeNetwork[, 3])))
    cc4 <- paste0(
      vars[variables$idxNodes[which(
        variables$exp[variables$idxNodes] %in% kk)]], " - ", 
      vars[variables$idxB[which(
        variables$exp[variables$idxNodes] %in% kk)]], 
      " = 0")
  }
  
  constraints8 <- c(cc1, cc2, cc3, cc4)
  
  return(constraints8)
}

createLoopConstraints <- function( variables = variables, 
                                   perturbations = perturbations,
                                   priorKnowledgeNetwork = priorKnowledgeNetwork) {
  
  .Deprecated("createLoopConstraints_v2")
  M <- 101
  constraints1 <- c()
  constraints2 <- c()
  constraints3 <- c()
  constraints4 <- c()
  
  if(length(which(priorKnowledgeNetwork[, 3] %in% names(perturbations))) == 0){
    pkn <- priorKnowledgeNetwork
  }
  
  if(length(which(priorKnowledgeNetwork[, 3] %in% names(perturbations))) > 0){
    pkn <- priorKnowledgeNetwork[-which(priorKnowledgeNetwork[, 3] %in% names(perturbations)), ]
  }
  
  reactionsUp <- 
    variables$variables[which(
      variables$exp%in%paste0(
        "ReactionUp ", 
        pkn[, 1], "=", pkn[, 3]))]
  reactionsDown <- 
    variables$variables[which(
      variables$exp%in%paste0(
        "ReactionDown ", pkn[, 1], "=", pkn[, 3]))]
  
  ##
  if(length(which(priorKnowledgeNetwork[, 3] %in% names(perturbations))) > 0){
    kk <- sapply(
      strsplit(
        variables$exp[variables$idxEdgesUp], 
        split = " "), function(x) x[2])[-which(
          priorKnowledgeNetwork[, 3] %in% names(perturbations))]
  }
  if(length(which(priorKnowledgeNetwork[, 3] %in% names(perturbations))) == 0){
    kk <- sapply(
      strsplit(
        variables$exp[variables$idxEdgesUp], split = " "), 
      function(x) x[2])
  }
  cc <- paste0(M, " ", reactionsUp, " + dist_", 
               sapply(strsplit(kk, split = "="), function(x) x[1]), 
               " - dist_", sapply(strsplit(kk, split = "="), 
                                  function(x) x[2]), " <= ", M-1)
  constraints1 <- c(constraints1, cc)
  
  ##
  if(length(which(priorKnowledgeNetwork[, 3] %in% names(perturbations))) > 0){
    kk <- sapply(
      strsplit(
        variables$exp[variables$idxEdgesDown], 
        split = " "), 
      function(x) x[2])[-which(priorKnowledgeNetwork[, 3] %in% names(perturbations))]
  }
  
  if(length(which(priorKnowledgeNetwork[, 3] %in% names(perturbations))) == 0){
    kk <- sapply(
      strsplit(
        variables$exp[variables$idxEdgesDown], 
        split = " "), function(x) x[2])
  }
  
  cc <- paste0(M, " ", reactionsDown, " + dist_", 
               sapply(strsplit(kk, split = "="), 
                      function(x) x[1]), " - dist_", 
               sapply(strsplit(kk, split = "="), 
                      function(x) x[2]), " <= ", M-1)
  constraints2 <- c(constraints2, cc)
  
  return(c(constraints1, constraints2))
  
}
