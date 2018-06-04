readOutResults <- function(cplexSolutionFileName, variables = variables, pknList=pknList, conditionIDX = conditionIDX, dir_name = dir_name, Export_all = Export_all,inputs = inputs, measurements = measurements){
  
  cplexSolutionData <- xmlParse(cplexSolutionFileName)
  cplexSolution <- xmlToList(cplexSolutionData)
  
  sifAll <- list()
  nodesAll <- list()
  
  for(ii in 2:(length(cplexSolution)-1)){
    
    x1 = lapply(cplexSolution[[ii]][[4]], "[[", 1)
    vars <- unlist(x1)
    
    idxNodes <- which(vars%in%variables[[conditionIDX]]$variables[variables[[conditionIDX]]$idxNodes])
    idxNodesUp <- which(vars%in%variables[[conditionIDX]]$variables[variables[[conditionIDX]]$idxNodesUp])
    idxNodesDown <- which(vars%in%variables[[conditionIDX]]$variables[variables[[conditionIDX]]$idxNodesDown])
    idxEdgesUp <- which(vars%in%variables[[conditionIDX]]$variables[variables[[conditionIDX]]$idxEdgesUp])
    idxEdgesDown <- which(vars%in%variables[[conditionIDX]]$variables[variables[[conditionIDX]]$idxEdgesDown])
    
    x2 = lapply(cplexSolution[[ii]][[4]], "[[", 3)
    values <- unlist(x2)
    
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
    
    if(class(edgesDown) != "matrix"){
      
      edgesDown <- as.matrix(t(edgesDown))
      
    }
    
    if(class(edgesUp) != "matrix"){
      
      edgesUp <- as.matrix(t(edgesUp))
      
    }
    
    # Writing SIF and DOT files
    
    pknList <- as.matrix(pknList)
    sif <- matrix(data = "", nrow = 1, ncol = 3)
    colnames(sif) <- colnames(pknList)
    
    Dot_text <- NULL
    Dot_text <- c(Dot_text,"digraph {")
    Dot_text <- c(Dot_text,"")
    
    kk1 <- as.numeric(which(edgesUp[, 2] == 1))
    if(length(kk1) > 0){
      
      for(i in 1:length(kk1)){
        
        ss <- strsplit(strsplit(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==edgesUp[kk1[i], 1])], split = " ")[[1]][2], split = "=")[[1]][1]
        tt <- strsplit(strsplit(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==edgesUp[kk1[i], 1])], split = " ")[[1]][2], split = "=")[[1]][2]
        
        if(as.numeric(edgesUp[kk1[i], 2])==as.numeric(nodesUp[which(nodesUp[, 1]==variables[[conditionIDX]]$variables[which(variables[[conditionIDX]]$exp==paste0("SpeciesUP ", tt, " in experiment ", conditionIDX))]), 2])){
          
          sif <- rbind(sif, pknList[kk1[i], ])
          Dot_text <- c(Dot_text,paste0(ss,"->",tt," [penwidth=",toString(1),", color=black]"))
          
        }
        
      }
      
    }
    
    
    kk1 <- as.numeric(which(edgesDown[, 2] == 1))
    if(length(kk1) > 0){
      
      for(i in 1:length(kk1)){
        
        ss <- strsplit(strsplit(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==edgesDown[kk1[i], 1])], split = " ")[[1]][2], split = "=")[[1]][1]
        tt <- strsplit(strsplit(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==edgesDown[kk1[i], 1])], split = " ")[[1]][2], split = "=")[[1]][2]
        
        if(as.numeric(edgesDown[kk1[i], 2])==as.numeric(nodesDown[which(nodesDown[, 1]==variables[[conditionIDX]]$variables[which(variables[[conditionIDX]]$exp==paste0("SpeciesDown ", tt, " in experiment ", conditionIDX))]), 2])){
          
          sif <- rbind(sif, pknList[kk1[i], ])
          Dot_text <- c(Dot_text,paste0(ss,"->",tt," [penwidth=",toString(1),", color=red]"))
          
        }
        
      }
      
    }
    
    if (nrow(sif)==2) { # If there is only one interaction (plus one empty line)
      # convert it to matrix, remove empty line, and convert back to matrix for exporting
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
    
    idx <- intersect(which(nodesAct[, 2] != "0"), which(nodesAct[, 2] != "-0"))
    
    if (length(idx)!=0) {
      
      activityNodes <- matrix(data = , nrow = length(idx), ncol = 2)
      activityNodes[, 2] <- nodesAct[idx, 2]
      for(i in 1:length(idx)){
        
        activityNodes[i, 1] <- strsplit(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==nodesAct[idx[i], 1])], split = " ")[[1]][2]
        
      }
      colnames(activityNodes) <- c("Nodes","Activity")
      
    } else if (length(idx)==0) {
      activityNodes = "All node activities are 0"
    }
    
    # Write SIF, DOT and Nodes' activities files
    
    if (!is.null(sif)) {
      write.table(x = sif, file = paste0("results/",dir_name,"/interactions_", conditionIDX, "_model", ii, ".tsv"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(x = "Empty network returned", file = paste0("results/",dir_name,"/interactions_", "_model", ii, conditionIDX, ".tsv"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
    }
    
    if (length(idx)!=0) {
      write.table(x = activityNodes, file = paste0("results/",dir_name,"/nodesActivity_", conditionIDX, "_model", ii, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(x = activityNodes, file = paste0("results/",dir_name,"/nodesActivity_", conditionIDX, "_model", ii, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
    }
    
    # Map DOT figure (only when SIF network is present)
    
    if (!is.null(sif)) {
      ColorNode <- c("black","red")
      ColorNodeAll <- c("lavender","mistyrose")
      IdxMapped <- NULL
      
      
      # Screen for only inputs in the final SIF network
      AllNodeSIF <- unique(c(sif[,1],sif[,3]))
      AllInputs <- colnames(inputs)
      inputsName <- NULL
      for (counter in 1:length(AllInputs)) {
        if (sum(AllInputs[counter]==AllNodeSIF)>0) {
          inputsName <- c(inputsName,AllInputs[counter])
        }
      }
      
      # Map input(s)' activities
      for (counter in 1:length(inputsName)) {
        if (length(which(inputsName[counter]==activityNodes[,1]))>0) {
          IdxInput <- which(inputsName[counter]==activityNodes[,1])
          Dot_text <- c(Dot_text,paste0(inputsName[counter]," [style=filled, color=",
                                        if (activityNodes[IdxInput,2]>0) {paste0(ColorNode[1],", fillcolor=",ColorNodeAll[1])} 
                                        else if (activityNodes[IdxInput,2]<0) {paste0(ColorNode[2],", fillcolor=",ColorNodeAll[2])},", shape=invhouse];"))
          IdxMapped <- c(IdxMapped, IdxInput)
        }
      }
      
      # Map measurement(s)' activities
      AllMeas <- colnames(measurements)
      measName <- NULL
      for (counter in 1:length(AllMeas)) {
        if (sum(AllMeas[counter]==AllNodeSIF)>0) {
          measName <- c(measName,AllMeas[counter])
        }
      }
      
      for (counter in 1:length(measName)) {
        if (length(which(measName[counter]==activityNodes[,1]))>0) {
          IdxMeas <- which(measName[counter]==activityNodes[,1])
          Dot_text <- c(Dot_text,paste0(measName[counter]," [style=filled, color=",
                                        if (activityNodes[IdxMeas,2]>0) {paste0(ColorNode[1],", fillcolor=",ColorNodeAll[1])} 
                                        else if (activityNodes[IdxMeas,2]<0) {paste0(ColorNode[2],", fillcolor=",ColorNodeAll[2])},", shape=doublecircle];"))
          IdxMapped <- c(IdxMapped, IdxMeas)
        }
      }
      
      
      # Map the rest of nodes activities
      RemainingNodeIdx <- NULL
      if(length(IdxMapped) > 0){
        RemainingNodeIdx <- (1:nrow(activityNodes))[-IdxMapped]
      }
      RemainingNodeName <- activityNodes[RemainingNodeIdx,1]
      
      RestOfNodeIdx <- NULL
      
      if (length(RemainingNodeIdx)>0) {
        for (counter in 1:length(RemainingNodeName)) {
          if (sum(RemainingNodeName[counter]==AllNodeSIF)>0) {
            RestOfNodeIdx <- c(RestOfNodeIdx,which(RemainingNodeName[counter]==activityNodes[,1]))
          }
        }
      } else {
        RestOfNodeIdx <- NULL
      }
      if (length(RestOfNodeIdx)>0) {
        for (counter in 1:length(RestOfNodeIdx)) {
          Dot_text <- c(Dot_text,paste0(activityNodes[RestOfNodeIdx[counter],1]," [style=filled, fillcolor=",
                                        if (activityNodes[RestOfNodeIdx[counter],2]>0) {ColorNodeAll[1]}
                                        else if (activityNodes[RestOfNodeIdx[counter],2]<0) {ColorNodeAll[2]},"];"))
        }
      }
      
      
      Dot_text <- c(Dot_text,"")
      Dot_text <- c(Dot_text,"")
      Dot_text <- c(Dot_text,"}")
      
      fileConn <- file(paste0("results/",dir_name,"/ActivityNetwork_", conditionIDX, "_model", ii, ".dot"))
      writeLines(Dot_text,fileConn)
      close(fileConn)
    } 
    
    if (Export_all) {
      write.table(x = nodes, file = paste0("results/",dir_name,"/nodesAttributes_", conditionIDX, "_model", ii, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
      write.table(x = nodesUp, file = paste0("results/",dir_name,"/nodesUpAttributes_", conditionIDX, "_model", ii, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
      write.table(x = nodesDown, file = paste0("results/",dir_name,"/nodesDownAttributes_", conditionIDX, "_model", ii, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
      write.table(x = edgesUp, file = paste0("results/",dir_name,"/reactionsUpAttributes_", conditionIDX, "_model", ii, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
      write.table(x = edgesDown, file = paste0("results/",dir_name,"/reactionsDownAttributes_", conditionIDX, "_model", ii, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
    
    sifAll[[length(sifAll)+1]] <- sif
    nodesAll[[length(nodesAll)+1]] <- nodes
    
  }
  
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
  
  write.table(x = weightedSIF, file = "weightedModel.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  
  ##
  nodesVar <- c()
  for(ii in 1:length(nodesAll)){
    
    nodesVar <- unique(c(nodesVar, unique(nodesAll[[ii]][, 1])))
    
  }
  
  nodesNames <- c()
  var <- variables[[conditionIDX]]
  for(ii in 1:length(nodesVar)){
    
    nodesNames <- c(nodesNames, strsplit(x = var$exp[which(var$variables==nodesVar[ii])], split = " ")[[1]][2])
    
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
      
      if(nodesAll[[j]][idx, 2]=="0"){
        
        zeroCnt <- zeroCnt + 1
        
      }
      
      if(nodesAll[[j]][idx, 2]=="1"){
        
        upCnt <- upCnt + 1
        
      }
      
      if(nodesAll[[j]][idx, 2]=="-1"){
        
        downCnt <- downCnt + 1
        
      }
      
    }
    
    nodesAttributes[i, 2] <- as.character(zeroCnt*100/length(nodesAll))
    nodesAttributes[i, 3] <- as.character(upCnt*100/length(nodesAll))
    nodesAttributes[i, 4] <- as.character(downCnt*100/length(nodesAll))
    nodesAttributes[i, 5] <- as.character((zeroCnt*0+upCnt*1+downCnt*(-1))*100/length(nodesAll))
    
    if(nodesAttributes[i, 1]%in%colnames(measurements)){
      
      nodesAttributes[i, 6] <- "P"
      
    } else {
      
      if(nodesAttributes[i, 1]%in%colnames(inputs)){
        
        nodesAttributes[i, 6] <- "D"
        
      } else {
        
        nodesAttributes[i, 6] <- ""
        
      }
      
    }
    
  }
  
  colnames(nodesAttributes) <- c("Node", "ZeroAct", "UpAct", "DownAct", "AvgAct", "nodesP")
  
  write.table(x = nodesAttributes, file = "nodesAttributes.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  
  RES <- list()
  RES[[length(RES)+1]] <- weightedSIF
  RES[[length(RES)+1]] <- nodesAttributes
  
  names(RES) <- c("weightedSIF", "nodesAttributes")
  
  return(RES)
  
}
