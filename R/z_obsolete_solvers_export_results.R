## Obsolete code kept here for backward compatibility for v.2.1. 
# Planned to be removed in v.3 
exportIlpSolutionResultFromXml <- function(solMatrix = solMatrix,
                                           variables = variables, 
                                           dataPreprocessed = dataPreprocessed){
  
  .Deprecated("exportIlpSolutionFromSolutionMatrix")
  priorKnowledgeNetwork <- dataPreprocessed$priorKnowledgeNetwork
  perturbations <- dataPreprocessed$perturbations
  measurements <- dataPreprocessed$measurement
  
  #TODO for lpSolve and cbc this is like the string below
  #vars <- solMatrix$name
  vars <- rownames(solMatrix)
  internalVariables <- variables$variables
  
  sifAll <- list()
  nodesAll <- list()
  nodesActAll <- list()
  
  idxNodes <- 
    which(vars %in% internalVariables[variables$idxNodes])
  idxNodesUp <- 
    which(vars %in% internalVariables[variables$idxNodesUp])
  idxNodesDown <- 
    which(vars %in% internalVariables[variables$idxNodesDown])
  idxEdgesUp <- 
    which(vars %in% internalVariables[variables$idxEdgesUp])
  idxEdgesDown <- 
    which(vars %in% internalVariables[variables$idxEdgesDown])
  
  indeces <- c(idxNodes, idxNodesUp, idxNodesDown, idxEdgesUp, idxEdgesDown)
  
  solMatrix <- as.matrix(solMatrix)
  
  #TODO for lpSolve and cbc this line is below
  #for(ii in seq(from = 2, to = ncol(solMatrix), by = 1)){
  
  for (ii in seq_len(ncol(solMatrix))) {
    values <- solMatrix[, ii]
    
    valNodes <- as.numeric(values[idxNodes])
    valNodesDown <- as.numeric(values[idxNodesDown])
    valNodesUp <- as.numeric(values[idxNodesUp])
    valEdgesUp <- as.numeric(values[idxEdgesUp])
    valEdgesDown <- as.numeric(values[idxEdgesDown])
    
    nodes <- matrix(data = "", nrow = length(idxNodes), ncol = 2)
    nodesUp <- matrix(data = "", nrow = length(idxNodesUp), ncol = 2)
    nodesDown <- matrix(data = "", nrow = length(idxNodesDown), ncol = 2)
    edgesUp <- matrix(data = "", nrow = length(idxEdgesUp), ncol = 2)
    edgesDown <- matrix(data = "", nrow = length(idxEdgesDown), ncol = 2)
    ctrl <- 0
    
    colnames(nodes) <- c("variable", "value")
    colnames(nodesUp) <- c("variable", "value")
    colnames(nodesDown) <- c("variable", "value")
    colnames(edgesUp) <- c("variable", "value")
    colnames(edgesDown) <- c("variable", "value")
    
    nodes[, 1] <- vars[idxNodes]
    nodes[, 2] <- as.numeric(values[idxNodes])
    
    nodesUp[, 1] <- vars[idxNodesUp]
    nodesUp[, 2] <- as.numeric(values[idxNodesUp])
    
    nodesDown[, 1] <- vars[idxNodesDown]
    nodesDown[, 2] <- as.numeric(values[idxNodesDown])
    
    edgesUp[, 1] <- vars[idxEdgesUp]
    edgesUp[, 2] <- as.numeric(values[idxEdgesUp])
    
    edgesDown[, 1] <- vars[idxEdgesDown]
    edgesDown[, 2] <- as.numeric(values[idxEdgesDown])
    
    nodes <- matrix(data = "", nrow = length(idxNodes), ncol = 2)
    nodesUp <- matrix(data = "", nrow = length(idxNodesUp), ncol = 2)
    nodesDown <- matrix(data = "", nrow = length(idxNodesDown), ncol = 2)
    edgesUp <- matrix(data = "", nrow = length(idxEdgesUp), ncol = 2)
    edgesDown <- matrix(data = "", nrow = length(idxEdgesDown), ncol = 2)
    ctrl <- 0
    
    colnames(nodes) <- c("variable", "value")
    colnames(nodesUp) <- c("variable", "value")
    colnames(nodesDown) <- c("variable", "value")
    colnames(edgesUp) <- c("variable", "value")
    colnames(edgesDown) <- c("variable", "value")
    
    nodes[, 1] <- vars[idxNodes]
    nodes[, 2] <- as.numeric(values[idxNodes])
    nodesUp[, 1] <- vars[idxNodesUp]
    nodesUp[, 2] <- as.numeric(values[idxNodesUp])
    nodesDown[, 1] <- vars[idxNodesDown]
    nodesDown[, 2] <- as.numeric(values[idxNodesDown])
    edgesUp[, 1] <- vars[idxEdgesUp]
    edgesUp[, 2] <- as.numeric(values[idxEdgesUp])
    edgesDown[, 1] <- vars[idxEdgesDown]
    edgesDown[, 2] <- as.numeric(values[idxEdgesDown])
    
    if(!is(edgesDown, "matrix")){
      edgesDown <- as.matrix(t(edgesDown))
    }
    
    if(!is(edgesUp, "matrix")){
      edgesUp <- as.matrix(t(edgesUp))
    }
    
    # Writing network results
    priorKnowledgeNetwork <- as.matrix(priorKnowledgeNetwork)
    sif <- matrix(data = "", nrow = 1, ncol = 3)
    colnames(sif) <- colnames(priorKnowledgeNetwork)
    
    kk1 <- as.numeric(which(edgesUp[, 2] >= 0.99))
    if(length(kk1) > 0){
      
      for(i in seq_len(length(kk1))){
        
        ss <- 
          strsplit(
            strsplit(
              variables$exp[which(
                internalVariables == edgesUp[kk1[i], 1])], 
              split = " ")[[1]][2], split = "=")[[1]][1]
        tt <- 
          strsplit(
            strsplit(
              variables$exp[which(
                internalVariables == edgesUp[kk1[i], 1])], 
              split = " ")[[1]][2], split = "=")[[1]][2]
        
        
        if(round(as.numeric(edgesUp[kk1[i], 2])) == round(as.numeric(
          nodesUp[which(
            nodesUp[, 1] == internalVariables[which(
              variables$exp == paste0("SpeciesUP ", tt))]), 2]))){
          
          sif <- rbind(sif, priorKnowledgeNetwork[kk1[i], ])
          
        }
        
      }
      
    }
    
    
    kk1 <- as.numeric(which(edgesDown[, 2] >= 0.99))
    if(length(kk1) > 0){
      
      for(i in seq_len(length(kk1))){
        
        ss <- 
          strsplit(strsplit(
            variables$exp[which(
              internalVariables == edgesDown[kk1[i], 1])], 
            split = " ")[[1]][2], split = "=")[[1]][1]
        tt <- 
          strsplit(strsplit(
            variables$exp[which(
              internalVariables == edgesDown[kk1[i], 1])], 
            split = " ")[[1]][2], split = "=")[[1]][2]
        
        if(round(as.numeric(edgesDown[kk1[i], 2])) == round(as.numeric(
          nodesDown[which(
            nodesDown[, 1] == internalVariables[which(
              variables$exp == paste0("SpeciesDown ", tt))]), 2]))){
          
          sif <- rbind(sif, priorKnowledgeNetwork[kk1[i], ])
          
        }
        
      }
      
    }
    
    if (nrow(sif) == 2) {
      ## If there is only one interaction (plus one empty line)
      ## convert it to matrix, remove empty line, and convert back to matrix 
      ## for exporting
      sif <- as.matrix(sif)
      sif <- sif[-1,]
      if (is.vector(sif)) {
        sif <- t(as.matrix(sif))
      }
    } else {
      sif <- sif[-1,] ## simply remove an empty line
    }
    
    if (nrow(sif) == 0) {
      sif <- NULL
    }
    
    ## Node activities
    
    nodesAct <- nodes
    colnames(nodesAct) <- c("Nodes","Activity")
    
    idx <- intersect(which(nodesAct[, 2] != "0"), 
                     which(nodesAct[, 2] != "-0"))
    
    if (length(idx)!=0) {
      
      activityNodes <- matrix(data = , nrow = length(idx), ncol = 2)
      activityNodes[, 2] <- nodesAct[idx, 2]
      for(i in seq_len(length(idx))){
        
        activityNodes[i, 1] <- 
          strsplit(variables$exp[which(
            internalVariables == nodesAct[idx[i], 1])], 
            split = " ")[[1]][2]
        
      }
      colnames(activityNodes) <- c("Nodes","Activity")
      
    } else if (length(idx) == 0) {
      activityNodes = "All node activities are 0"
    }
    
    sifAll[[length(sifAll)+1]] <- sif
    nodesAll[[length(nodesAll)+1]] <- nodes
    nodesActAll[[length(nodesActAll)+1]] <- activityNodes
    
  }
  
  if(length(sifAll) == 0){
    message("No network was generated for this setting..")
    return(NULL)
  } else {
    for(ii in seq_len(length(sifAll))){
      if(ii == 1){
        SIF <- sifAll[[ii]]
      } else {
        SIF <- unique(rbind(SIF, sifAll[[ii]]))
      }
    }
    
    ##
    weightedSIF <- matrix(data = , nrow = nrow(SIF), ncol = 4)
    weightedSIF[, seq_len(3)] <- SIF
    for(i in seq_len(nrow(SIF))){
      
      cnt <- 0
      
      for(j in seq_len(length(sifAll))){
        
        idxNode1 <- which(sifAll[[j]][, 1] == SIF[i, 1])
        idxSign <- which(sifAll[[j]][, 2] == SIF[i, 2])
        idxNode2 <- which(sifAll[[j]][, 3] == SIF[i, 3])
        
        idx1 <- intersect(idxNode1, idxNode2)
        if(length(idx1) > 0){
          
          idx2 <- intersect(idxSign, idx1)
          
          if(length(idx2) > 0){
            
            cnt <- cnt + 1
            
          }
          
        }
        
      }
      
      weightedSIF[i, 4] <- as.character(cnt*100/length(sifAll))
      
    }
    
    colnames(weightedSIF) <- c("Node1", "Sign", "Node2", "Weight")
    #TODO fix an extra space
    #weightedSIF$Sign <- as.numeric(weightedSIF$Sign)
    
    ##
    nodesVar <- c()
    for(ii in seq_len(length(nodesAll))){
      
      nodesVar <- unique(c(nodesVar, unique(nodesAll[[ii]][, 1])))
      
    }
    
    nodesNames <- c()
    var <- variables
    for(ii in seq_len(length(nodesVar))){
      
      nodesNames <- c(nodesNames, 
                      strsplit(x = var$exp[which(
                        var$variables == nodesVar[ii])], split = " ")[[1]][2])
      
    }
    
    nodesAttributes <- matrix(data = , nrow = length(nodesNames), ncol = 6)
    nodesAttributes[, 1] <- nodesNames
    for(i in seq_len(nrow(nodesAttributes))){
      
      zeroCnt <- 0
      upCnt <- 0
      downCnt <- 0
      
      for(j in seq_len(length(nodesAll))){
        
        currVar <- nodesVar[i]
        
        idx <- which(nodesAll[[j]][, 1] == currVar)
        
        if(round(as.numeric(nodesAll[[j]][idx, 2])) == 0){
          
          zeroCnt <- zeroCnt + 1
          
        }
        
        if(round(as.numeric(nodesAll[[j]][idx, 2])) == 1){
          
          upCnt <- upCnt + 1
          
        }
        
        if(round(as.numeric(nodesAll[[j]][idx, 2])) == -1){
          
          downCnt <- downCnt + 1
          
        }
        
      }
      
      nodesAttributes[i, 2] <- as.character(zeroCnt*100/length(nodesAll))
      nodesAttributes[i, 3] <- as.character(upCnt*100/length(nodesAll))
      nodesAttributes[i, 4] <- as.character(downCnt*100/length(nodesAll))
      nodesAttributes[i, 5] <- 
        as.character((zeroCnt*0+upCnt*1+downCnt*(-1))*100/length(nodesAll))
      
      if(nodesAttributes[i, 1] %in% names(measurements)){
        
        nodesAttributes[i, 6] <- "T"
        
      } else {
        
        if(nodesAttributes[i, 1] %in% names(perturbations)){
          
          nodesAttributes[i, 6] <- "S"
          
        } else {
          
          nodesAttributes[i, 6] <- ""
          
        }
        
      }
      
    }
    
    colnames(nodesAttributes) <- c("Node", "ZeroAct", "UpAct", 
                                   "DownAct", "AvgAct", "NodeType")
    
    result <- list("weightedSIF" = weightedSIF, 
                   "nodesAttributes" = nodesAttributes,
                   "sifAll" = sifAll, 
                   "attributesAll" = nodesActAll)
    
    return(result)
    
  }
  
}