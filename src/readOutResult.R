readOutResult <- function(cplexSolutionFileName, variables = variables, pknList=pknList, conditionIDX = conditionIDX, dir_name = dir_name, Export_all = Export_all){
  
  cplexSolutionData <- xmlParse(cplexSolutionFileName)
  cplexSolution <- xmlToList(cplexSolutionData)
  
  sif <- matrix(data = "", nrow = 1, ncol = 3)
  nodes <- matrix(data = "", nrow = 1, ncol = 3)
  nodesUp <- matrix(data = "", nrow = 1, ncol = 3)
  nodesDown <- matrix(data = "", nrow = 1, ncol = 3)
  edgesUp <- matrix(data = "", nrow = 1, ncol = 3)
  edgesDown <- matrix(data = "", nrow = 1, ncol = 3)
  ctrl <- 0
  
  for(i in 1:length(cplexSolution$variables)){
    
    if(strsplit(cplexSolution$variables[[i]][1], split = "_")[[1]][2] == as.character(conditionIDX) && !grepl(pattern = "absDiff", x = cplexSolution$variables[[i]][1])){
      
      if(strsplit(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==cplexSolution$variables[[i]][1])], split = " ")[[1]][1]=="Species"){
        
        nodes <- rbind(nodes, c(cplexSolution$variables[[i]][1], 
                                gsub(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==cplexSolution$variables[[i]][1])], pattern = paste0(" in experiment ", conditionIDX), replacement = ""),
                                as.numeric(cplexSolution$variables[[i]][3])))
        
      }
      
      if(strsplit(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==cplexSolution$variables[[i]][1])], split = " ")[[1]][1]=="SpeciesUP"){
        
        nodesUp <- rbind(nodesUp, c(cplexSolution$variables[[i]][1], 
                                    gsub(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==cplexSolution$variables[[i]][1])], pattern = paste0(" in experiment ", conditionIDX), replacement = ""),
                                    as.numeric(cplexSolution$variables[[i]][3])))
        
      }
      
      if(strsplit(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==cplexSolution$variables[[i]][1])], split = " ")[[1]][1]=="SpeciesDown"){
        
        nodesDown <- rbind(nodesDown, c(cplexSolution$variables[[i]][1], 
                                        gsub(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==cplexSolution$variables[[i]][1])], pattern = paste0(" in experiment ", conditionIDX), replacement = ""),
                                        as.numeric(cplexSolution$variables[[i]][3])))
        
      }
      
      if(strsplit(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==cplexSolution$variables[[i]][1])], split = " ")[[1]][1]=="ReactionUp"){
        
        edgesUp <- rbind(edgesUp, c(cplexSolution$variables[[i]][1], 
                                    gsub(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==cplexSolution$variables[[i]][1])], pattern = paste0(" in experiment ", conditionIDX), replacement = ""),
                                    as.numeric(cplexSolution$variables[[i]][3])))
        
      }
      
      if(strsplit(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==cplexSolution$variables[[i]][1])], split = " ")[[1]][1]=="ReactionDown"){
        
        edgesDown <- rbind(edgesDown, c(cplexSolution$variables[[i]][1], 
                                        gsub(variables[[conditionIDX]]$exp[which(variables[[conditionIDX]]$variables==cplexSolution$variables[[i]][1])], pattern = paste0(" in experiment ", conditionIDX), replacement = ""),
                                        as.numeric(cplexSolution$variables[[i]][3])))
        
      }
      
    }
    
  }
  
  colnames(nodes) <- c("variable", "node", "value")
  colnames(nodesUp) <- c("variable", "node", "value")
  colnames(nodesDown) <- c("variable", "node", "value")
  colnames(edgesUp) <- c("variable", "edge", "value")
  colnames(edgesDown) <- c("variable", "edge", "value")
  
  nodes <- nodes[-1, ]
  nodesUp <- nodesUp[-1, ]
  nodesDown <- nodesDown[-1, ]
  edgesUp <- edgesUp[-1, ]
  edgesDown <- edgesDown[-1, ]
  
  if(class(edgesDown) != "matrix"){
    
    edgesDown <- as.matrix(t(edgesDown))
    
  }
  
  if(class(edgesUp) != "matrix"){
    
    edgesUp <- as.matrix(t(edgesUp))
    
  }
  
  nodes[, 2] <- unlist(strsplit(nodes[, 2], split = " "))[c(FALSE, TRUE)]
  nodesDown[, 2] <- unlist(strsplit(nodesDown[, 2], split = " "))[c(FALSE, TRUE)]
  nodesUp[, 2] <- unlist(strsplit(nodesUp[, 2], split = " "))[c(FALSE, TRUE)]
  edgesDown[, 2] <- unlist(strsplit(edgesDown[, 2], split = " "))[c(FALSE, TRUE)]
  edgesUp[, 2] <- unlist(strsplit(edgesUp[, 2], split = " "))[c(FALSE, TRUE)]

  sif <- matrix(data = "", nrow = 1, ncol = 3)
  kk1 <- as.numeric(which(edgesUp[, 3] == 1))
  if(length(kk1) > 0){
    
    for(i in 1:length(kk1)){
      
      ss <- strsplit(gsub(pattern = "ReactionUp ", replacement = "", x = edgesUp[kk1[i], 2]), split = "=")[[1]][1]
      tt <- strsplit(gsub(pattern = "ReactionUp ", replacement = "", x = edgesUp[kk1[i], 2]), split = "=")[[1]][2]
      
      # if((nodes[which(nodes[, 2]==paste0("Species ", ss)), 3] != "0") && (nodes[which(nodes[, 2]==paste0("Species ", tt)), 3] != "0")){
      #   
      #   sif <- rbind(sif, as.matrix(pknList[intersect(which(as.character(pknList$Node1)==ss), which(as.character(pknList$Node2)==tt)), ]))
      #   
      # }
      
      if((as.numeric(edgesUp[which(edgesUp[, 2]==paste0(ss, "=", tt)), 3])==as.numeric(nodesUp[which(nodesUp[, 2]==tt), 3]))){
        
        sif <- rbind(sif, as.matrix(pknList[intersect(which(as.character(pknList$Node1)==ss), which(as.character(pknList$Node2)==tt)), ]))
        
      }
      
    }
    
  }
  kk1 <- as.numeric(which(edgesDown[, 3] == 1))
  if(length(kk1) > 0){
    
    for(i in 1:length(kk1)){
      
      ss <- strsplit(gsub(pattern = "ReactionDown ", replacement = "", x = edgesDown[kk1[i], 2]), split = "=")[[1]][1]
      tt <- strsplit(gsub(pattern = "ReactionDown ", replacement = "", x = edgesDown[kk1[i], 2]), split = "=")[[1]][2]
      
      # if((nodes[which(nodes[, 2]==paste0("Species ", ss)), 3] != "0") && (nodes[which(nodes[, 2]==paste0("Species ", tt)), 3] != "0") && 
      #    (nodes[which(nodes[, 2]==paste0("Species ", ss)), 3] != "-0") && (nodes[which(nodes[, 2]==paste0("Species ", tt)), 3] != "-0")){
      #   
      #   sif <- rbind(sif, as.matrix(pknList[intersect(which(as.character(pknList$Node1)==ss), which(as.character(pknList$Node2)==tt)), ]))
      #   
      # }
      
      if((as.numeric(edgesDown[which(edgesDown[, 2]==paste0(ss, "=", tt)), 3])==as.numeric(nodesDown[which(nodesDown[, 2]==tt), 3]))){
        
        sif <- rbind(sif, as.matrix(pknList[intersect(which(as.character(pknList$Node1)==ss), which(as.character(pknList$Node2)==tt)), ]))
        
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
  
  nodesAct <- nodes[,-1]
  colnames(nodesAct) <- c("Nodes","Activity")
  
  write.table(x = sif, file = paste0("results/",dir_name,"/interactions_", conditionIDX, ".sif"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  write.table(x = nodesAct, file = paste0("results/",dir_name,"/nodesActivity_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  
  if (Export_all) {
    write.table(x = nodes, file = paste0("results/",dir_name,"/nodesAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    write.table(x = nodesUp, file = paste0("results/",dir_name,"/nodesUpAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    write.table(x = nodesDown, file = paste0("results/",dir_name,"/nodesDownAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    write.table(x = edgesUp, file = paste0("results/",dir_name,"/reactionsUpAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    write.table(x = edgesDown, file = paste0("results/",dir_name,"/reactionsDownAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  }

}
