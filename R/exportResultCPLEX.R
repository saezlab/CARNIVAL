## Extract and export the optimisation results from the cplex solution file 
## (XML) as files and variables for further plotting functions - CPLEX
##
## Enio Gjerga & Panuwat Trairatphisan, 2020

exportResultCPLEX <- function(cplexSolutionFileName = cplexSolutionFileName, 
                              variables = variables, 
                              conditionIDX = conditionIDX,
                              pknList = pknList, 
                              inputs=inputs, 
                              measurements=measurement){
  
  solution <- read.delim(file = cplexSolutionFileName)
  solution[, 1] <- as.character(solution[, 1])
  idxVarStart <- which(grepl(pattern = "<variables>", x = solution[, 1]))[-1]
  idxVarEnd <- which(grepl(pattern = "</variables>", x = solution[, 1]))[-1]
  
  solMatrix <- matrix(data = , nrow = idxVarEnd[1]-idxVarStart[1]-1, 
                      ncol = length(idxVarStart))
  colnames(solMatrix) <- paste0("Solution-", 1:ncol(solMatrix))
  ss1 <- sapply(strsplit(solution[(idxVarStart[1]+1):(idxVarEnd[1]-1), 1], 
                         split = " "), "[", 5)
  rownames(solMatrix) <- sapply((strsplit(ss1, split = "=")), "[", 2)
  
  for(ii in 1:ncol(solMatrix)){
    
    ss1 <- 
      sapply(strsplit(solution[(idxVarStart[ii]+1):(idxVarEnd[ii]-1), 1], 
                      split = " "), "[", 7)
    solMatrix[, ii] <- 
      gsub(pattern = "/>", replacement = "", 
           x = sapply(strsplit(ss1, split = "="), "[", 2))
    
  }
  
  sifAll <- list()
  nodesAll <- list()
  nodesActAll <- list()
  vars <- rownames(solMatrix)
  
  idxNodes <- 
    which(
      vars%in%variables[[conditionIDX]]$variables[
        variables[[conditionIDX]]$idxNodes])
  idxNodesUp <- 
    which(
      vars%in%variables[[conditionIDX]]$variables[
        variables[[conditionIDX]]$idxNodesUp])
  idxNodesDown <- 
    which(vars%in%variables[[conditionIDX]]$variables[
      variables[[conditionIDX]]$idxNodesDown])
  idxEdgesUp <- 
    which(vars%in%variables[[conditionIDX]]$variables[
      variables[[conditionIDX]]$idxEdgesUp])
  idxEdgesDown <- 
    which(vars%in%variables[[conditionIDX]]$variables[
      variables[[conditionIDX]]$idxEdgesDown])
  
  indeces <- c(idxNodes, idxNodesUp, idxNodesDown, idxEdgesUp, idxEdgesDown)
  
  for(ii in 1:ncol(solMatrix)){
    
    values <- solMatrix[, ii]
    
    valNodes <- values[idxNodes]
    valNodesDown <- values[idxNodesDown]
    valNodesUp <- values[idxNodesUp]
    valEdgesUp <- values[idxEdgesUp]
    valEdgesDown <- values[idxEdgesDown]
    
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
    nodes[, 2] <- values[idxNodes]
    nodesUp[, 1] <- vars[idxNodesUp]
    nodesUp[, 2] <- values[idxNodesUp]
    nodesDown[, 1] <- vars[idxNodesDown]
    nodesDown[, 2] <- values[idxNodesDown]
    edgesUp[, 1] <- vars[idxEdgesUp]
    edgesUp[, 2] <- values[idxEdgesUp]
    edgesDown[, 1] <- vars[idxEdgesDown]
    edgesDown[, 2] <- values[idxEdgesDown]
    
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
    nodes[, 2] <- values[idxNodes]
    nodesUp[, 1] <- vars[idxNodesUp]
    nodesUp[, 2] <- values[idxNodesUp]
    nodesDown[, 1] <- vars[idxNodesDown]
    nodesDown[, 2] <- values[idxNodesDown]
    edgesUp[, 1] <- vars[idxEdgesUp]
    edgesUp[, 2] <- values[idxEdgesUp]
    edgesDown[, 1] <- vars[idxEdgesDown]
    edgesDown[, 2] <- values[idxEdgesDown]
    
    if(!is(edgesDown, "matrix")){
      
      edgesDown <- as.matrix(t(edgesDown))
      
    }
    
    if(!is(edgesUp, "matrix")){
      
      edgesUp <- as.matrix(t(edgesUp))
      
    }
    
    # Writing SIF and DOT files
    
    pknList <- as.matrix(pknList)
    sif <- matrix(data = "", nrow = 1, ncol = 3)
    colnames(sif) <- colnames(pknList)
    
    kk1 <- as.numeric(which(edgesUp[, 2] >= 0.99))
    if(length(kk1) > 0){
      
      for(i in 1:length(kk1)){
        
        ss <- 
          strsplit(
            strsplit(
              variables[[conditionIDX]]$exp[which(
                variables[[conditionIDX]]$variables==edgesUp[kk1[i], 1])], 
              split = " ")[[1]][2], split = "=")[[1]][1]
        tt <- 
          strsplit(
            strsplit(
              variables[[conditionIDX]]$exp[which(
                variables[[conditionIDX]]$variables==edgesUp[kk1[i], 1])], 
              split = " ")[[1]][2], split = "=")[[1]][2]
        
        
        if(round(as.numeric(edgesUp[kk1[i], 2]))==
           round(as.numeric(nodesUp[which(nodesUp[, 1]==
                                    variables[[conditionIDX]]$variables[which(
                                      variables[[conditionIDX]]$exp==paste0(
                                        "SpeciesUP ", tt, " in experiment ", 
                                        conditionIDX))]), 2]))){
          
          sif <- rbind(sif, pknList[kk1[i], ])
          
        }
        
      }
      
    }
    
    
    kk1 <- as.numeric(which(edgesDown[, 2] >= 0.99))
    if(length(kk1) > 0){
      
      for(i in 1:length(kk1)){
        
        ss <- strsplit(
          strsplit(
            variables[[conditionIDX]]$exp[which(
              variables[[conditionIDX]]$variables==edgesDown[kk1[i], 1])], 
            split = " ")[[1]][2], split = "=")[[1]][1]
        tt <- strsplit(
          strsplit(
            variables[[conditionIDX]]$exp[which(
              variables[[conditionIDX]]$variables==edgesDown[kk1[i], 1])], 
            split = " ")[[1]][2], split = "=")[[1]][2]
        
        if(round(as.numeric(edgesDown[kk1[i], 2]))==
           round(as.numeric(nodesDown[which(
             nodesDown[, 1]==variables[[conditionIDX]]$variables[which(
               variables[[conditionIDX]]$exp==paste0("SpeciesDown ", tt, 
                                                     " in experiment ", 
                                                     conditionIDX))]), 2]))){
          
          sif <- rbind(sif, pknList[kk1[i], ])
          
        }
        
      }
      
    }
    
    if (nrow(sif)==2) {
      ## If there is only one interaction (plus one empty line)
      ## convert it to matrix, remove empty line, and convert back to matrix 
      ## for exporting
      sif <- as.matrix(sif)
      sif <- sif[-1,]
      if (is.vector(sif)) {
        sif <- t(as.matrix(sif))
      }
    } else {
      sif <- sif[-1,] # simply remove an empty line
    }
    
    if (nrow(sif)==0) {
      sif <- NULL
    }
    
    # Node activities
    
    nodesAct <- nodes
    colnames(nodesAct) <- c("Nodes","Activity")
    
    idx <- intersect(which(nodesAct[, 2] != "0"), 
                     which(nodesAct[, 2] != "-0"))
    
    if (length(idx)!=0) {
      
      activityNodes <- matrix(data = , nrow = length(idx), ncol = 2)
      activityNodes[, 2] <- nodesAct[idx, 2]
      for(i in 1:length(idx)){
        
        activityNodes[i, 1] <- 
          strsplit(
            variables[[conditionIDX]]$exp[which(
              variables[[conditionIDX]]$variables==nodesAct[idx[i], 1])], 
            split = " ")[[1]][2]
        
      }
      colnames(activityNodes) <- c("Nodes","Activity")
      
    } else if (length(idx)==0) {
      activityNodes = "All node activities are 0"
    }
    
    # Write SIF, DOT and Nodes' activities files
    sifAll[[length(sifAll)+1]] <- sif
    nodesAll[[length(nodesAll)+1]] <- nodes
    nodesActAll[[length(nodesActAll)+1]] <- activityNodes
    
  }
  
  # print(sifAll)
  
  if(length(sifAll)==0){
    
    print("No network was generated for this setting..")
    
    RES <- NULL
    
    return(RES)
    
  } else {
    
    for(ii in 1:length(sifAll)){
      
      if(ii ==1){
        
        SIF <- sifAll[[ii]]
        
      } else {
        
        SIF <- unique(rbind(SIF, sifAll[[ii]]))
        
      }
      
    }
    
    ##
    weightedSIF <- matrix(data = , nrow = nrow(SIF), ncol = 4)
    weightedSIF[, 1:3] <- SIF
    for(i in 1:nrow(SIF)){
      
      cnt <- 0
      
      for(j in 1:length(sifAll)){
        
        idxNode1 <- which(sifAll[[j]][, 1]==SIF[i, 1])
        idxSign <- which(sifAll[[j]][, 2]==SIF[i, 2])
        idxNode2 <- which(sifAll[[j]][, 3]==SIF[i, 3])
        
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
    
    ##
    nodesVar <- c()
    for(ii in 1:length(nodesAll)){
      
      nodesVar <- unique(c(nodesVar, unique(nodesAll[[ii]][, 1])))
      
    }
    
    nodesNames <- c()
    var <- variables[[conditionIDX]]
    for(ii in 1:length(nodesVar)){
      
      nodesNames <- c(nodesNames, 
                      strsplit(
                        x = var$exp[which(var$variables==nodesVar[ii])], 
                        split = " ")[[1]][2])
      
    }
    
    nodesAttributes <- matrix(data = , nrow = length(nodesNames), ncol = 6)
    nodesAttributes[, 1] <- nodesNames
    for(i in 1:nrow(nodesAttributes)){
      
      zeroCnt <- 0
      upCnt <- 0
      downCnt <- 0
      
      for(j in 1:length(nodesAll)){
        
        currVar <- nodesVar[i]
        
        idx <- which(nodesAll[[j]][, 1]==currVar)
        
        if(round(as.numeric(nodesAll[[j]][idx, 2]))==0){
          
          zeroCnt <- zeroCnt + 1
          
        }
        
        if(round(as.numeric(nodesAll[[j]][idx, 2]))==1){
          
          upCnt <- upCnt + 1
          
        }
        
        if(round(as.numeric(nodesAll[[j]][idx, 2]))==-1){
          
          downCnt <- downCnt + 1
          
        }
        
      }
      
      nodesAttributes[i, 2] <- as.character(zeroCnt*100/length(nodesAll))
      nodesAttributes[i, 3] <- as.character(upCnt*100/length(nodesAll))
      nodesAttributes[i, 4] <- as.character(downCnt*100/length(nodesAll))
      nodesAttributes[i, 5] <- 
        as.character((zeroCnt*0+upCnt*1+downCnt*(-1))*100/length(nodesAll))
      
      if(nodesAttributes[i, 1]%in%colnames(measurements)){
        
        nodesAttributes[i, 6] <- "T"
        
      } else {
        
        if(nodesAttributes[i, 1]%in%colnames(inputs)){
          
          nodesAttributes[i, 6] <- "S"
          
        } else {
          
          nodesAttributes[i, 6] <- ""
          
        }
        
      }
      
    }
    
    colnames(nodesAttributes) <- c("Node", "ZeroAct", "UpAct", 
                                   "DownAct", "AvgAct", "NodeType")
    
    RES <- list()
    RES[[length(RES)+1]] <- weightedSIF
    RES[[length(RES)+1]] <- nodesAttributes
    RES[[length(RES)+1]] <- sifAll
    RES[[length(RES)+1]] <- nodesActAll
    
    names(RES) <- c("weightedSIF", "nodesAttributes","sifAll","attributesAll")
    
    return(RES)
    
  }
  
}