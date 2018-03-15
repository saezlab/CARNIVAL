readOutResult <- function(cplexSolutionFileName, variables = variables, pknList=pknList, conditionIDX = conditionIDX, dir_name = dir_name, Export_all = Export_all,inputs = inputs, measurements = measurements){
  
  cplexSolutionData <- xmlParse(cplexSolutionFileName)
  cplexSolution <- xmlToList(cplexSolutionData)
  
  x1 = lapply(cplexSolution[[4]], "[[", 1)
  vars <- unlist(x1)
  
  idxNodes <- which(vars%in%variables[[conditionIDX]]$variables[variables[[conditionIDX]]$idxNodes])
  idxNodesUp <- which(vars%in%variables[[conditionIDX]]$variables[variables[[conditionIDX]]$idxNodesUp])
  idxNodesDown <- which(vars%in%variables[[conditionIDX]]$variables[variables[[conditionIDX]]$idxNodesDown])
  idxEdgesUp <- which(vars%in%variables[[conditionIDX]]$variables[variables[[conditionIDX]]$idxEdgesUp])
  idxEdgesDown <- which(vars%in%variables[[conditionIDX]]$variables[variables[[conditionIDX]]$idxEdgesDown])
  
  x2 = lapply(cplexSolution[[4]], "[[", 3)
  values <- unlist(x2)
  
  valNodes <- values[idxNodes]
  valNodesDown <- values[idxNodesDown]
  valNodesUp <- values[idxNodesUp]
  valEdgesUp <- values[idxEdgesUp]
  valEdgesDown <- values[idxEdgesDown]
  
  # expNodes <- gsub(variables[[conditionIDX]]$exp[idxNodes], pattern = paste0(" in experiment ", conditionIDX), replacement = "")
  # expNodesDown <- gsub(variables[[conditionIDX]]$exp[idxNodesDown], pattern = paste0(" in experiment ", conditionIDX), replacement = "")
  # expNodesUp <- gsub(variables[[conditionIDX]]$exp[idxNodesUp], pattern = paste0(" in experiment ", conditionIDX), replacement = "")
  # expEdgesUp <- gsub(variables[[conditionIDX]]$exp[idxEdgesUp], pattern = paste0(" in experiment ", conditionIDX), replacement = "")
  
  # sif <- matrix(data = "", nrow = 1, ncol = 2)
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
  
  # nodes[, 2] <- unlist(strsplit(nodes[, 2], split = " "))[c(FALSE, TRUE)]
  # nodesDown[, 2] <- unlist(strsplit(nodesDown[, 2], split = " "))[c(FALSE, TRUE)]
  # nodesUp[, 2] <- unlist(strsplit(nodesUp[, 2], split = " "))[c(FALSE, TRUE)]
  # edgesDown[, 2] <- unlist(strsplit(edgesDown[, 2], split = " "))[c(FALSE, TRUE)]
  # edgesUp[, 2] <- unlist(strsplit(edgesUp[, 2], split = " "))[c(FALSE, TRUE)]
  
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
      
      # if((as.numeric(edgesUp[which(edgesUp[, 2]==paste0(ss, "=", tt)), 2])==as.numeric(nodesUp[which(nodesUp[, 2]==tt), 2]))){
      #   
      #   sif <- rbind(sif, as.matrix(pknList[intersect(which(as.character(pknList$Node1)==ss), which(as.character(pknList$Node2)==tt)), ]))
      #   
      # }
      
      # if(as.numeric(edgesUp[kk1[i], 2])==as.numeric(nodes[which(nodes[, 1]==variables[[conditionIDX]]$variables[which(variables[[conditionIDX]]$exp==paste0("Species ", tt, " in experiment ", conditionIDX))]), 2])){
      if(as.numeric(edgesUp[kk1[i], 2])==as.numeric(nodesUp[which(nodesUp[, 1]==variables[[conditionIDX]]$variables[which(variables[[conditionIDX]]$exp==paste0("SpeciesUP ", tt, " in experiment ", conditionIDX))]), 2])){
          
        # sif <- rbind(sif, as.matrix(pknList[intersect(which(as.character(pknList$Node1)==ss), which(as.character(pknList$Node2)==tt)), ]))
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
      
      # if((as.numeric(edgesDown[which(edgesDown[, 2]==paste0(ss, "=", tt)), 2])==as.numeric(nodesDown[which(nodesDown[, 2]==tt), 2]))){
      #   
      #   sif <- rbind(sif, as.matrix(pknList[intersect(which(as.character(pknList$Node1)==ss), which(as.character(pknList$Node2)==tt)), ]))
      #   
      # }
      
      # if(as.numeric(edgesDown[kk1[i], 2])==(-1)*as.numeric(nodes[which(nodes[, 1]==variables[[conditionIDX]]$variables[which(variables[[conditionIDX]]$exp==paste0("Species ", tt, " in experiment ", conditionIDX))]), 2])){
      if(as.numeric(edgesDown[kk1[i], 2])==as.numeric(nodesDown[which(nodesDown[, 1]==variables[[conditionIDX]]$variables[which(variables[[conditionIDX]]$exp==paste0("SpeciesDown ", tt, " in experiment ", conditionIDX))]), 2])){
          
        # sif <- rbind(sif, as.matrix(pknList[intersect(which(as.character(pknList$Node1)==ss), which(as.character(pknList$Node2)==tt)), ]))
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
    # activityNodes[, 1] <- unlist(lapply(strsplit(variables[[conditionIDX]]$exp[match(variables[[conditionIDX]]$variables, nodesAct[idx, 1])], split = " "), "[[", 2))
    for(i in 1:length(idx)){
      
      activityNodes[i, 1] <- strsplit(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==nodesAct[idx[i], 1])], split = " ")[[1]][2]
      
    }
    colnames(activityNodes) <- c("Nodes","Activity")
  
  } else if (length(idx)==0) {
    activityNodes = "All node activities are 0"
  }
  
  # Write SIF, DOT and Nodes' activities files
  
  if (!is.null(sif)) {
    write.table(x = sif, file = paste0("results/",dir_name,"/interactions_", conditionIDX, ".tsv"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  } else {
    write.table(x = "Empty network returned", file = paste0("results/",dir_name,"/interactions_", conditionIDX, ".tsv"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  }
  
  if (length(idx)!=0) {
    write.table(x = activityNodes, file = paste0("results/",dir_name,"/nodesActivity_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  } else {
    write.table(x = activityNodes, file = paste0("results/",dir_name,"/nodesActivity_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
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
  
    RemainingNodeIdx <- (1:nrow(activityNodes))[-IdxMapped]
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
    
    fileConn <- file(paste0("results/",dir_name,"/ActivityNetwork_", conditionIDX, ".dot"))
    writeLines(Dot_text,fileConn)
    close(fileConn)
  } 
  
  if (Export_all) {
    write.table(x = nodes, file = paste0("results/",dir_name,"/nodesAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    write.table(x = nodesUp, file = paste0("results/",dir_name,"/nodesUpAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    write.table(x = nodesDown, file = paste0("results/",dir_name,"/nodesDownAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    write.table(x = edgesUp, file = paste0("results/",dir_name,"/reactionsUpAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    write.table(x = edgesDown, file = paste0("results/",dir_name,"/reactionsDownAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  
  return(sif)
  
}
