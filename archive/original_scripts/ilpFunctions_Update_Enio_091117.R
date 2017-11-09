##
buildDataMatrix <- function(data = data, pknList = pknList, inputs = inputs, cutoff = cutoff){
  
  colnames(pknList) <- c("X1", "X2", "X3")
  allSpecies <- unique(c(as.character(pknList$X1), as.character(pknList$X3)))
  
  if(ncol(inputs) > 0){
    
    ts <- intersect(colnames(inputs), allSpecies)
    
  }
  
  ds <- intersect(colnames(data), allSpecies)
  
  dn <- setdiff(allSpecies, ds)
  
  dataMatrix <- matrix(0, nrow = nrow(data), ncol = length(allSpecies))
  
  dnNames <- paste0("DN:", dn)
  dsNames <- paste0("DS:", ds)
  
  colnames(dataMatrix) <- c(dnNames, dsNames)
  
  if(length(which(is.element(el = colnames(data), set = setdiff(colnames(data), ds)))) > 0){
    
    dataMatrix[, (length(dn)+1):length(allSpecies)] <- as.matrix(data[, -which(is.element(el = colnames(data), set = setdiff(colnames(data), ds)))])
    
  }
  else{
    
    dataMatrix[, (length(dn)+1):length(allSpecies)] <- as.matrix(data)
    
  }
  
  dataMatrix[which(abs(dataMatrix)<cutoff, arr.ind = TRUE)] <- 0
  
  dataMatrixSign <- sign(dataMatrix)
  
  dnID <- 1:length(dn)
  dsID <- (length(dn)+1):length(allSpecies)
  tsID <- which(is.element(el = c(dn, ds), set = ts))
  
  res <- list(dataMatrix=dataMatrix, dataMatrixSign=dataMatrixSign, dnID=dnID, dsID=dsID, tsID=tsID, species=c(dn, ds))
  
  return(res)
  
}

##
create_variables <- function(pknList=pknList, dataMatrix = dataMatrix, conditionIDX=conditionIDX){
  
  colnames(pknList) <- c("X1", "X2", "X3")
  
  nodes <- paste0("xb", 1:(nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)), "_", conditionIDX)
  nodesUp <- paste0("xb", (nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)), "_", conditionIDX)
  nodesDown <- paste0("xb", (2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(3*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)), "_", conditionIDX)
  
  expNodes <- c()
  expNodesUp <- c()
  expNodesDown <- c()
  expNodesReduced <- c()
  expNodesReducedUpSource <- c()
  expNodesReducedDownSource <- c()
  expNodesReducedUpTarget <- c()
  expNodesReducedDownTarget <- c()
  idxExperimentNodes <- c()
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    expNodes <- c(expNodes, paste0("Species ", dataMatrix$species, " in experiment ", conditionIDX))
    expNodesReduced = c(expNodesReduced, paste0("Species ", dataMatrix$species))
    expNodesUp <- c(expNodesUp, paste0("SpeciesUP ", dataMatrix$species, " in experiment ", conditionIDX))
    expNodesDown <- c(expNodesDown, paste0("SpeciesDown ", dataMatrix$species, " in experiment ", conditionIDX))
    expNodesReducedUpSource <- c(expNodesReducedUpSource, as.character(pknList$X1))
    expNodesReducedDownSource <- c(expNodesReducedDownSource, as.character(pknList$X1))
    expNodesReducedUpTarget <- c(expNodesReducedUpTarget, as.character(pknList$X3))
    expNodesReducedDownTarget <- c(expNodesReducedDownTarget, as.character(pknList$X3))
    
    idxExperimentNodes <- c(idxExperimentNodes, length(expNodes))
    
  }
  
  nodesALL <- c(nodes, nodesUp, nodesDown)
  expNodesALL <- c(expNodes, expNodesUp, expNodesDown)
  
  idxNodes <- 1:(nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))
  idxNodesUp <- (nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))
  idxNodesDown <- (2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(3*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))
  
  # edges
  edgesUp <- paste0("xb", (length(nodesALL)+1):(length(nodesALL)+nrow(pknList)*nrow(dataMatrix$dataMatrix)), "_", conditionIDX)
  edgesDown <- paste0("xb", (length(nodesALL)+nrow(pknList)*nrow(dataMatrix$dataMatrix)+1):(length(nodesALL)+nrow(pknList)*nrow(dataMatrix$dataMatrix)+nrow(pknList)*nrow(dataMatrix$dataMatrix)), "_", conditionIDX)
  
  expEdgesUp <- c()
  expEdgesDown <- c()
  expEdgesReducedSource <- c()
  expEdgesReducedTarget <- c()
  idxExperimentEdges <- c()
  expNodesReducedUp <- c()
  expNodesReducedDown <- c()
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    expEdgesUp <- c(expEdgesUp, paste0("ReactionUp ", as.character(pknList$X1), "=", as.character(pknList$X3), " in experiment ", conditionIDX))
    expEdgesDown <- c(expEdgesDown, paste0("ReactionDown ", as.character(pknList$X1), "=", as.character(pknList$X3), " in experiment ", conditionIDX))
    expEdgesReducedSource <- c(expEdgesReducedSource, paste0("ReactionSource ", as.character(pknList$X1)))
    expEdgesReducedTarget <- c(expEdgesReducedTarget, paste0("ReactionTarget ", as.character(pknList$X3)))
    expNodesReducedUp <- c(expNodesReducedUp, pknList$X1)
    expNodesReducedDown <- c(expNodesReducedDown, pknList$X3)
    
    idxExperimentEdges <- c(idxExperimentEdges, length(expEdgesUp))
    
  }
  
  edgesALL <- c(edgesUp, edgesDown)
  expEdgesALL <- c(expEdgesUp, expEdgesDown)
  
  idxEdgesUp <- (length(nodesALL)+1):(length(nodesALL)+1+nrow(pknList)*nrow(dataMatrix$dataMatrix)-1)
  idxEdgesDown <- (length(nodesALL)+1+nrow(pknList)*nrow(dataMatrix$dataMatrix)):(length(nodesALL)+1+nrow(pknList)*nrow(dataMatrix$dataMatrix)+nrow(pknList)*nrow(dataMatrix$dataMatrix)-1)
  
  signs <- pknList$X2
  reactionSource <- as.character(pknList$X1)
  reactionTarget <- as.character(pknList$X3)
  if(nrow(dataMatrix$dataMatrix) > 1){
    
    for(i in 2:nrow(dataMatrix$dataMatrix)){
      
      signs <- c(signs, pknList$X2)
      reactionSource <- c(reactionSource, as.character(pknList$X1))
      reactionTarget <- c(reactionTarget, as.character(pknList$X3))
      
    }
    
  }
  
  # output
  res <- list(variables=c(nodesALL, edgesALL), exp=c(expNodesALL, expEdgesALL), idxNodes=idxNodes, idxNodesUp=idxNodesUp, 
              idxNodesDown=idxNodesDown, idxEdgesUp=idxEdgesUp, idxEdgesDown=idxEdgesDown, signs=signs,
              reactionSource=reactionSource, reactionTarget=reactionTarget, expNodesReduced=expNodesReduced,
              expNodesReducedUpSource=expNodesReducedUpSource, expNodesReducedDownSource=expNodesReducedDownSource,
              expNodesReducedDownTarget=expNodesReducedDownTarget, expNodesReducedUpTarget=expNodesReducedUpTarget,
              expEdgesReducedSource=expEdgesReducedSource, expEdgesReducedTarget=expEdgesReducedTarget,
              idxExperimentNodes=idxExperimentNodes, idxExperimentEdges=idxExperimentEdges,
              expNodesReducedUp=expNodesReducedUp, expNodesReducedDown=expNodesReducedDown)
  
  return(res)
  
}


##
create_variables_all <- function(pknList=pknList, dataMatrix=dataMatrix){
  
  res <- list()
  namesRes <- c()
  
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    dM <- dataMatrix
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))
    
    res[[length(res)+1]] <- create_variables(pknList = pknList, dataMatrix = dM, conditionIDX = i)
    
    namesRes <- c(namesRes, paste0("Condition_", i))
    
  }
  
  names(res) <- namesRes
  
  return(res)
  
}

##
write_boundaries <- function(variables=variables){
  
  bounds <- c()
  
  for(i in 1:length(variables)){
    
    bounds <- c(bounds, paste0("\t", "-1 <= ", variables[[i]]$variables[variables[[i]]$idxNodes], " <= 1"))
    bounds <- c(bounds, paste0("\t", "0 <= ", variables[[i]]$variables[variables[[i]]$idxNodesUp], " <= 1"))
    bounds <- c(bounds, paste0("\t", "0 <= ", variables[[i]]$variables[variables[[i]]$idxNodesDown], " <= 1"))
    bounds <- c(bounds, paste0("\t", "0 <= ", variables[[i]]$variables[variables[[i]]$idxEdgesUp], " <= 1"))
    bounds <- c(bounds, paste0("\t", "0 <= ", variables[[i]]$variables[variables[[i]]$idxEdgesDown], " <= 1"))
    
  }
  
  return(bounds)
  
}

##
write_objective_function <- function(dataMatrix = dataMatrix, variables = variables, alpha=alpha, beta=beta){
  
  measured <- gsub(colnames(dataMatrix$dataMatrix)[dataMatrix$dsID], pattern = "DS:", replacement = "")
  
  idxMeasured <- c()
  for(i in 1:length(measured)){
    
    idxMeasured <- c(idxMeasured, which(variables$expNodesReduced==paste0("Species ", measured[i])))
    
  }
  
  measuredVar <- variables$variables[idxMeasured]
  
  
  objectiveFunctionVec <- paste0(" + ", alpha, " absDiff", gsub(measuredVar, pattern = "xb", replacement = ""))
  objectiveFunction <- paste(objectiveFunctionVec, collapse = "")
  
  objectiveFunction <- substring(text = objectiveFunction[1], first = 4, last = nchar(objectiveFunction))
  
  objectiveFunction <- paste0("", objectiveFunction)
  
  objectiveFunctionUpVec <- paste0(" + ", beta, " ", variables$variables[variables$idxNodesUp])
  objectiveFunctionUp <- paste(objectiveFunctionUpVec, collapse = "")
  
  objectiveFunctionDownVec <- paste0(" + ", beta, " ", variables$variables[variables$idxNodesDown])
  objectiveFunctionDown <- paste(objectiveFunctionDownVec, collapse = "")
  
  objectiveFunction <- paste0(objectiveFunction, objectiveFunctionUp)
  objectiveFunction <- paste0(objectiveFunction, objectiveFunctionDown)
  
  return(objectiveFunction)
  
}

##
write_objective_function_all <- function(dataMatrix = dataMatrix, variables = variables, alpha=alpha, beta=beta){
  
  OF <- "Obj:\t"
  
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    dM <- dataMatrix
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))
    
    var <- variables[[i]]
    
    if(i==1){
      
      OF <- paste0(OF, write_objective_function(dataMatrix = dM, variables = var, alpha = alpha, beta = beta))
      
    }
    else{
      
      OF <- paste0(OF, " + ",  write_objective_function(dataMatrix = dM, variables = var, alpha = alpha, beta = beta))
      
    }
    
  }
  
  return(OF)
  
}

##
write_constraints_objFunction <- function(variables=variables, dataMatrix=dataMatrix, conditionIDX=conditionIDX){
  
  measurements <- as.vector(t(dataMatrix$dataMatrixSign))
  
  idx2 <- which(measurements==1)
  idx3 <- which(measurements==-1)
  
  cc1 <- rep("", length(measurements))
  cc2 <- rep("", length(measurements))
  
  cc1[idx2] <- paste0(variables$variables[idx2], " - absDiff", idx2, "_", conditionIDX, " <= 1")
  cc2[idx2] <- paste0(variables$variables[idx2], " + absDiff", idx2, "_", conditionIDX, " >= 1")
  
  cc1[idx3] <- paste0(variables$variables[idx3], " - absDiff", idx3, "_", conditionIDX, " <= -1")
  cc2[idx3] <- paste0(variables$variables[idx3], " + absDiff", idx3, "_", conditionIDX, " >= -1")
  
  constraints0 <- c(cc1, cc2)
  
  return(constraints0[-which(constraints0=="")])
  
}

##
write_constraints_objFunction_all <- function(variables=variables, dataMatrix=dataMatrix){
  
  constraints0 <- c()
  
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    dM <- dataMatrix
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))
    
    var <- variables[[i]]
    
    constraints0 <- c(constraints0, write_constraints_objFunction(variables = var, dataMatrix = dM, conditionIDX = i))
    
  }
  
  return(constraints0)
  
}

##
write_constraints_1 <- function(variables=variables, conditionIDX=conditionIDX){
  
  constraints1 <- rep("", length(variables$idxEdgesUp))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints1[idx1] <- paste0(variables$variables[variables$idxEdgesUp[idx1]], " - ",
                               variables$variables[match(paste0("Species ",
                                                          unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesUp[idx1]], pattern = "ReactionUp ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                          " in experiment ", conditionIDX), variables$exp)], " >= 0")
  
  constraints1[idx2] <- paste0(variables$variables[variables$idxEdgesUp[idx2]], " + ",
                               variables$variables[match(paste0("Species ",
                                                                unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesUp[idx2]], pattern = "ReactionUp ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                " in experiment ", conditionIDX), variables$exp)], " >= 0")
  
  return(constraints1)
  
}

##
write_constraints_1_all <- function(variables=variables){
  
  constraints1 <- c()
  
  for(i in 1:length(variables)){
    
    var <- variables[[i]]
    
    constraints1 <- c(constraints1, write_constraints_1(variables = var, conditionIDX = i))
    
  }
  
  return(constraints1)
  
}

##
write_constraints_2 <- function(variables=variables, conditionIDX=conditionIDX){
  
  constraints1 <- rep("", length(variables$idxEdgesDown))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints1[idx1] <- paste0(variables$variables[variables$idxEdgesDown[idx1]], " + ",
                               variables$variables[match(paste0("Species ",
                                                                unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesDown[idx1]], pattern = "ReactionDown ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                " in experiment ", conditionIDX), variables$exp)], " >= 0")
  
  constraints1[idx2] <- paste0(variables$variables[variables$idxEdgesDown[idx2]], " - ",
                               variables$variables[match(paste0("Species ",
                                                                unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesDown[idx2]], pattern = "ReactionDown ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                " in experiment ", conditionIDX), variables$exp)], " >= 0")
  
  return(constraints1)
  
}

##
write_constraints_2_all <- function(variables=variables){
  
  constraints1 <- c()
  
  for(i in 1:length(variables)){
    
    var <- variables[[i]]
    
    constraints1 <- c(constraints1, write_constraints_2(variables = var, conditionIDX = i))
    
  }
  
  return(constraints1)
  
}

##
write_constraints_3 <- function(variables=variables){
  
  constraints3 <- paste0(variables$variables[variables$idxEdgesUp], " + ", variables$variables[variables$idxEdgesDown], " <= 1")
  
  return(constraints3)
  
}

##
write_constraints_3_all <- function(variables=variables){
  
  constraints3 <- c()
  
  for(i in 1:length(variables)){
    
    var <- variables[[i]]
    
    constraints3 <- c(constraints3, write_constraints_3(variables = var))
    
  }
  
  return(constraints3)
  
}

##
write_constraints_4 <- function(variables=variables, conditionIDX=conditionIDX){
  
  constraints1 <- rep("", length(variables$idxEdgesUp))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints1[idx1] <- paste0(variables$variables[variables$idxEdgesUp[idx1]], " - ",
                               variables$variables[match(paste0("Species ",
                                                                unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesUp[idx1]], pattern = "ReactionUp ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                " in experiment ", conditionIDX), variables$exp)], " - ", 
                               variables$variables[match(gsub(variables$exp[variables$idxEdgesUp[idx1]], pattern = "ReactionUp ", replacement = "ReactionDown "), 
                                                         variables$exp)],
                               " <= 0")
  
  constraints1[idx2] <- paste0(variables$variables[variables$idxEdgesUp[idx2]], " + ",
                               variables$variables[match(paste0("Species ",
                                                                unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesUp[idx2]], pattern = "ReactionUp ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                " in experiment ", conditionIDX), variables$exp)], " - ",
                               variables$variables[match(gsub(variables$exp[variables$idxEdgesUp[idx2]], pattern = "ReactionUp ", replacement = "ReactionDown "), 
                                                         variables$exp)],
                               " >= 0")
  
  return(constraints1)
  
}

##
write_constraints_4_all <- function(variables=variables){
  
  constraints4 <- c()
  
  for(i in 1:length(variables)){
    
    var <- variables[[i]]
    
    constraints4 <- c(constraints4, write_constraints_4(variables = var, conditionIDX = i))
    
  }
  
  return(constraints4)
  
}

##
write_constraints_5 <- function(variables=variables, conditionIDX=conditionIDX){
  
  constraints1 <- rep("", length(variables$idxEdgesDown))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints1[idx1] <- paste0(variables$variables[variables$idxEdgesDown[idx1]], " + ",
                               variables$variables[match(paste0("Species ",
                                                                unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesDown[idx1]], pattern = "ReactionDown ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                " in experiment ", conditionIDX), variables$exp)], " - ", 
                               variables$variables[match(gsub(variables$exp[variables$idxEdgesDown[idx1]], pattern = "ReactionDown ", replacement = "ReactionUp "), 
                                                         variables$exp)],
                               " <= 0")
  
  constraints1[idx2] <- paste0(variables$variables[variables$idxEdgesDown[idx2]], " - ",
                               variables$variables[match(paste0("Species ",
                                                                unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesDown[idx2]], pattern = "ReactionDown ", replacement = ""), pattern = paste0(" in experiment ", conditionIDX), replacement = ""), split = "="))[c(TRUE, FALSE)],
                                                                " in experiment ", conditionIDX), variables$exp)], " - ",
                               variables$variables[match(gsub(variables$exp[variables$idxEdgesDown[idx2]], pattern = "ReactionDown ", replacement = "ReactionUp "), 
                                                         variables$exp)],
                               " >= 0")
  
  return(constraints1)
  
}

##
write_constraints_5_all <- function(variables=variables){
  
  constraints5 <- c()
  
  for(i in 1:length(variables)){
    
    var <- variables[[i]]
    
    constraints5 <- c(constraints5, write_constraints_5(variables = var, conditionIDX = i))
    
  }
  
  return(constraints5)
  
}

##
write_constraints_6 <- function(variables=variables, dataMatrix=dataMatrix){
  
  constraints6 <- c()
  
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    source <- unique(variables[[i]]$reactionSource)
    target <- unique(variables[[i]]$reactionTarget)
    
    for(ii in 1:length(target)){
      
      pp1 <- variables[[i]]$variables[which(variables[[i]]$exp==paste0("SpeciesUP ", target[ii], " in experiment ", i))]
      pp2 <- ""
      
      for(jj in 1:length(source)){
        
        reaction <- paste0("ReactionUp ", source[jj], "=", target[ii], " in experiment ", i)
        
        if(length(which(variables[[i]]$exp==reaction)) > 0){
          
          for(kk in 1:length(which(variables[[i]]$exp==reaction))){
            
            pp2 <- paste0(pp2, " - ", variables[[i]]$variables[which(variables[[i]]$exp==reaction)[kk]])
            
          }
          
        }
        
      }
      
      constraints6 <- c(constraints6, paste0(pp1, pp2, " <= 0"))
      
    }
    
  }
  
  return(constraints6)
  
}

##
write_constraints_7 <- function(variables=variables, dataMatrix=dataMatrix){
  
  constraints6 <- c()
  
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    source <- unique(variables[[i]]$reactionSource)
    target <- unique(variables[[i]]$reactionTarget)
    
    for(ii in 1:length(target)){
      
      pp1 <- variables[[i]]$variables[which(variables[[i]]$exp==paste0("SpeciesDown ", target[ii], " in experiment ", i))]
      pp2 <- ""
      
      for(jj in 1:length(source)){
        
        reaction <- paste0("ReactionDown ", source[jj], "=", target[ii], " in experiment ", i)
        
        if(length(which(variables[[i]]$exp==reaction)) > 0){
          
          for(kk in 1:length(which(variables[[i]]$exp==reaction))){
            
            pp2 <- paste0(pp2, " - ", variables[[i]]$variables[which(variables[[i]]$exp==reaction)[kk]])
            
          }
          
        }
        
      }
      
      constraints6 <- c(constraints6, paste0(pp1, pp2, " <= 0"))
      
    }
    
  }
  
  return(constraints6)
  
}

##
prepareInputs <- function(PROGENyTable = PROGENyTable, PROGENyProtein = PROGENyProtein, pknList = pknList, thresh = thresh){
  
  inputs <- matrix(data = 0, nrow = nrow(PROGENyTable), ncol = length(unique(c(pknList$Node1, pknList$Node2))))
  colnames(inputs) <- unique(c(pknList$Node1, pknList$Node2))
  rownames(inputs) <- rownames(PROGENyTable)
  
  for(i in 1:nrow(PROGENyTable)){
    
    for(j in 1:ncol(PROGENyTable)){
      
      if(abs(as.numeric(PROGENyTable[i, j])) > thresh){
        
        if(as.numeric(PROGENyTable[i, j]) < 0){
          
          inputs[i, which(colnames(inputs)==PROGENyProtein$Target[which(PROGENyProtein$Score==colnames(PROGENyTable)[j])])] <- -1
          
        }
        else{
          
          inputs[i, which(colnames(inputs)==PROGENyProtein$Target[which(PROGENyProtein$Score==colnames(PROGENyTable)[j])])] <- 1
          
        }
        
      }
      
    }
    
  }
  
  return(inputs)
  
}

##
write_constraints_8 <- function(variables=variables, inputs=inputs){
  
  constraints8 <- list()
  
  # inputs <- as.vector(t(inputs))
  
  inputsZero <- which(inputs==0, arr.ind = TRUE)
  inputsUp <- which(inputs==1, arr.ind = TRUE)
  inputsDown <- which(inputs==-1, arr.ind = TRUE)
  
  # ii <- as.vector(t(inputs))
  
  for(i in 1:length(variables)){
    
    constraints8[[i]] <- paste0(variables[[i]]$variables[variables[[i]]$idxNodesUp], " - ",
                           variables[[i]]$variables[variables[[i]]$idxNodesDown], " - ",
                           variables[[i]]$variables[variables[[i]]$idxNodes], " = 0")
    
  }
  
  if(nrow(inputsUp) > 0){
    
    for(ii in 1:length(variables)){
      
      for(i in 1:nrow(inputsUp)){
        
        ss1 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesUP ", colnames(inputs)[inputsUp[i, 2]], " in experiment ", inputsUp[i, 1]))]
        
        ss2 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesDown ", colnames(inputs)[inputsUp[i, 2]], " in experiment ", inputsUp[i, 1]))]
        
        ss3 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("Species ", colnames(inputs)[inputsUp[i, 2]], " in experiment ", inputsUp[i, 1]))]
        
        idx <- which(constraints8[[ii]]==paste0(ss1, " - ", ss2, " - ", ss3, " = 0"))
        
        constraints8[[ii]][idx] <- gsub(constraints8[[ii]][idx], pattern = " = 0", replacement = " = -1")
        
      }
      
    }
    
  }
  
  if(nrow(inputsDown) > 0){
    
    for(ii in 1:length(variables)){
      
      for(i in 1:nrow(inputsDown)){
        
        ss1 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesUP ", colnames(inputs)[inputsDown[i, 2]], " in experiment ", inputsDown[i, 1]))]
        
        ss2 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("SpeciesDown ", colnames(inputs)[inputsDown[i, 2]], " in experiment ", inputsDown[i, 1]))]
        
        ss3 <- variables[[ii]]$variables[which(variables[[ii]]$exp==paste0("Species ", colnames(inputs)[inputsDown[i, 2]], " in experiment ", inputsDown[i, 1]))]
        
        idx <- which(constraints8[[ii]]==paste0(ss1, " - ", ss2, " - ", ss3, " = 0"))
        
        constraints8[[ii]][idx] <- gsub(constraints8[[ii]][idx], pattern = " = 0", replacement = " = 1")
        
      }
      
    }
    
  }
  
  return(unlist(constraints8))
  
}

##
all_constraints <- function(c0=c0, c1=c1, c2=c2, c3=c3, c4=c4, c5=c5, c6=c6, c7=c7, c8=c8){
  
  allConst <- c(c0, c1, c2, c3, c4, c5, c6, c7, c8)
  
  allConstraints <- paste0("c", 1:length(allConst), ":\t", allConst, "\t \t")
  
  return(allConstraints)
  
}

# ##
# readOutResult <- function(cplexSolutionFileName, variables = variables, pknList=pknList){
#   
#   cplexSolutionData <- xmlParse(cplexSolutionFileName)
#   cplexSolution <- xmlToList(cplexSolutionData)
#   
#   sif <- matrix(data = "", nrow = 1, ncol = 3)
#   nodes <- matrix(data = "", nrow = 1, ncol = 3)
#   nodesUp <- matrix(data = "", nrow = 1, ncol = 3)
#   nodesDown <- matrix(data = "", nrow = 1, ncol = 3)
#   edgesUp <- matrix(data = "", nrow = 1, ncol = 3)
#   edgesDown <- matrix(data = "", nrow = 1, ncol = 3)
#   ctrl <- 0
#   
#   for(i in 1:length(cplexSolution$variables)){
#     
#     if(strsplit(cplexSolution$variables[[i]][1], split = "_")[[1]][2] == "1" && !grepl(pattern = "absDiff", x = cplexSolution$variables[[i]][1])){
#       
#       if(strsplit(variables[[1]]$exp[which(variables[[1]]$variables==cplexSolution$variables[[i]][1])], split = " ")[[1]][1]=="Species"){
#         
#         nodes <- rbind(nodes, c(cplexSolution$variables[[i]][1], 
#                        gsub(variables[[1]]$exp[which(variables[[1]]$variables==cplexSolution$variables[[i]][1])], pattern = " in experiment 1", replacement = ""),
#                        as.numeric(cplexSolution$variables[[i]][3])))
#         
#       }
#       
#       if(strsplit(variables[[1]]$exp[which(variables[[1]]$variables==cplexSolution$variables[[i]][1])], split = " ")[[1]][1]=="SpeciesUP"){
#         
#         nodesUp <- rbind(nodesUp, c(cplexSolution$variables[[i]][1], 
#                                 gsub(variables[[1]]$exp[which(variables[[1]]$variables==cplexSolution$variables[[i]][1])], pattern = " in experiment 1", replacement = ""),
#                                 as.numeric(cplexSolution$variables[[i]][3])))
#         
#       }
#       
#       if(strsplit(variables[[1]]$exp[which(variables[[1]]$variables==cplexSolution$variables[[i]][1])], split = " ")[[1]][1]=="SpeciesDown"){
#         
#         nodesDown <- rbind(nodesDown, c(cplexSolution$variables[[i]][1], 
#                                 gsub(variables[[1]]$exp[which(variables[[1]]$variables==cplexSolution$variables[[i]][1])], pattern = " in experiment 1", replacement = ""),
#                                 as.numeric(cplexSolution$variables[[i]][3])))
#         
#       }
#       
#       if(strsplit(variables[[1]]$exp[which(variables[[1]]$variables==cplexSolution$variables[[i]][1])], split = " ")[[1]][1]=="ReactionUp"){
#         
#         edgesUp <- rbind(edgesUp, c(cplexSolution$variables[[i]][1], 
#                                 gsub(variables[[1]]$exp[which(variables[[1]]$variables==cplexSolution$variables[[i]][1])], pattern = " in experiment 1", replacement = ""),
#                                 as.numeric(cplexSolution$variables[[i]][3])))
#         
#       }
#       
#       if(strsplit(variables[[1]]$exp[which(variables[[1]]$variables==cplexSolution$variables[[i]][1])], split = " ")[[1]][1]=="ReactionDown"){
#         
#         edgesDown <- rbind(edgesDown, c(cplexSolution$variables[[i]][1], 
#                                 gsub(variables[[1]]$exp[which(variables[[1]]$variables==cplexSolution$variables[[i]][1])], pattern = " in experiment 1", replacement = ""),
#                                 as.numeric(cplexSolution$variables[[i]][3])))
#         
#       }
#       
#     }
#     
#   }
#   
#   colnames(nodes) <- c("variable", "exp", "value")
#   colnames(nodesUp) <- c("variable", "exp", "value")
#   colnames(nodesDown) <- c("variable", "exp", "value")
#   colnames(edgesUp) <- c("variable", "exp", "value")
#   colnames(edgesDown) <- c("variable", "exp", "value")
#   
#   nodes <- nodes[-1, ]
#   nodesUp <- nodesUp[-1, ]
#   nodesDown <- nodesDown[-1, ]
#   edgesUp <- edgesUp[-1, ]
#   edgesDown <- edgesDown[-1, ]
#   
#   if(class(edgesDown) != "matrix"){
#     
#     edgesDown <- as.matrix(t(edgesDown))
#     
#   }
#   
#   if(class(edgesUp) != "matrix"){
#     
#     edgesUp <- as.matrix(t(edgesUp))
#     
#   }
#   
#   sif <- matrix(data = "", nrow = 1, ncol = 3)
#   kk1 <- as.numeric(which(edgesUp[, 3] == 1))
#   if(length(kk1) > 0){
#     
#     for(i in 1:length(kk1)){
#       
#       ss <- strsplit(gsub(pattern = "ReactionUp ", replacement = "", x = edgesUp[kk1[i], 2]), split = "=")[[1]][1]
#       tt <- strsplit(gsub(pattern = "ReactionUp ", replacement = "", x = edgesUp[kk1[i], 2]), split = "=")[[1]][2]
#       
#       if((nodes[which(nodes[, 2]==paste0("Species ", ss)), 3] != "0") && (nodes[which(nodes[, 2]==paste0("Species ", tt)), 3] != "0")){
#         
#         sif <- rbind(sif, as.matrix(pknList[intersect(which(as.character(pknList$Node1)==ss), which(as.character(pknList$Node2)==tt)), ]))
#         
#       }
#       
#     }
#     
#   }
#   kk1 <- as.numeric(which(edgesDown[, 3] == 1))
#   if(length(kk1) > 0){
#     
#     for(i in 1:length(kk1)){
#       
#       ss <- strsplit(gsub(pattern = "ReactionDown ", replacement = "", x = edgesDown[kk1[i], 2]), split = "=")[[1]][1]
#       tt <- strsplit(gsub(pattern = "ReactionDown ", replacement = "", x = edgesDown[kk1[i], 2]), split = "=")[[1]][2]
#       
#       if((nodes[which(nodes[, 2]==paste0("Species ", ss)), 3] != "0") && (nodes[which(nodes[, 2]==paste0("Species ", tt)), 3] != "0") && 
#          (nodes[which(nodes[, 2]==paste0("Species ", ss)), 3] != "-0") && (nodes[which(nodes[, 2]==paste0("Species ", tt)), 3] != "-0")){
#         
#         sif <- rbind(sif, as.matrix(pknList[intersect(which(as.character(pknList$Node1)==ss), which(as.character(pknList$Node2)==tt)), ]))
#         
#       }
#       
#     }
#     
#   }
#   
#   write.table(x = nodes, file = "nodesAttributes.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
#   write.table(x = nodesUp, file = "nodesUpAttributes.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
#   write.table(x = nodesDown, file = "nodesDownAttributes.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
#   write.table(x = edgesUp, file = "reactionsUpAttributes.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
#   write.table(x = edgesDown, file = "reactionDownAttributes.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
#   
#   write.table(x = sif, file = "interactions.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
#   
# }

##
readOutResult <- function(cplexSolutionFileName, variables = variables, pknList=pknList, conditionIDX = conditionIDX){
  
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
  
  colnames(nodes) <- c("variable", "exp", "value")
  colnames(nodesUp) <- c("variable", "exp", "value")
  colnames(nodesDown) <- c("variable", "exp", "value")
  colnames(edgesUp) <- c("variable", "exp", "value")
  colnames(edgesDown) <- c("variable", "exp", "value")
  
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
  
  write.table(x = nodes, file = paste0("nodesAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  write.table(x = nodesUp, file = paste0("nodesUpAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  write.table(x = nodesDown, file = paste0("nodesDownAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  write.table(x = edgesUp, file = paste0("reactionsUpAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  write.table(x = edgesDown, file = paste0("reactionsDownAttributes_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  
  # if(nrow(sif) > 1){
  #   
  #   sif <- sif[-1, ]
  #   
  # }
  # else{
  #   
  #   sif <- as.matrix(t(sif))
  #   
  # }
  
  sif <- sif[-1, ]
  
  write.table(x = sif, file = paste0("interactions_", conditionIDX, ".txt"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  
}



