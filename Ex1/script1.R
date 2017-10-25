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

  # for(i in 1:ncol(inputs)){
  #
  #   cIdx <- which(colnames(dataMatrix)==paste0("DN:", ts[i]))
  #   dataMatrix[, cIdx] <- inputs[, ts[i]]
  #
  # }

  dataMatrix[which(abs(dataMatrix)<cutoff, arr.ind = TRUE)] <- 0

  dataMatrixSign <- sign(dataMatrix)

  # if(nrow(dataMatrix) > 1){
  # 
  #   dataMatrix <- dataMatrix[apply(dataMatrix[,-1], 1, function(x) !all(x==0)),]
  #   dataMatrixSign <- dataMatrixSign[apply(dataMatrixSign[,-1], 1, function(x) !all(x==0)),]
  # 
  # }

  dnID <- 1:length(dn)
  dsID <- (length(dn)+1):length(allSpecies)
  tsID <- which(is.element(el = c(dn, ds), set = ts))

  res <- list(dataMatrix=dataMatrix, dataMatrixSign=dataMatrixSign, dnID=dnID, dsID=dsID, tsID=tsID, species=c(dn, ds))

  return(res)

}

##
create_variables <- function(pknList=pknList, dataMatrix = dataMatrix){
  
  colnames(pknList) <- c("X1", "X2", "X3")
  
  # species
  # nodes <- paste0("xb", 1:(nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)))
  # nodesUp <- paste0("xb", (nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)))
  # nodesDown <- paste0("xb", (2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(3*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)))
  nodes <- paste0("xb", 1:(nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)))
  nodesUp <- paste0("xb", (nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)))
  nodesDown <- paste0("xb", (2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(3*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)))
  
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
    
    expNodes <- c(expNodes, paste0("Species ", dataMatrix$species, " in experiment ", i))
    expNodesReduced = c(expNodesReduced, paste0("Species ", dataMatrix$species))
    expNodesUp <- c(expNodesUp, paste0("SpeciesUP ", dataMatrix$species, " in experiment ", i))
    expNodesDown <- c(expNodesDown, paste0("SpeciesDown ", dataMatrix$species, " in experiment ", i))
    expNodesReducedUpSource <- c(expNodesReducedUpSource, as.character(pknList$X1))
    expNodesReducedDownSource <- c(expNodesReducedDownSource, as.character(pknList$X1))
    expNodesReducedUpTarget <- c(expNodesReducedUpTarget, as.character(pknList$X3))
    expNodesReducedDownTarget <- c(expNodesReducedDownTarget, as.character(pknList$X3))
    
    idxExperimentNodes <- c(idxExperimentNodes, length(expNodes))
    
  }
  
  nodesALL <- c(nodes, nodesUp, nodesDown)
  expNodesALL <- c(expNodes, expNodesUp, expNodesDown)
  
  # idxNodes <- 1:(nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))
  # idxNodesUp <- (nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))
  # idxNodesDown <- (2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(3*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))
  # 
  # # edges
  # edgesUp <- paste0("xb", (length(nodesALL)+1):(length(nodesALL)+1+nrow(pknList)*nrow(dataMatrix$dataMatrix)))
  # edgesDown <- paste0("xb", (length(nodesALL)+1+nrow(pknList)*nrow(dataMatrix$dataMatrix)+1):(length(nodesALL)+1+nrow(pknList)*nrow(dataMatrix$dataMatrix)+nrow(pknList)*nrow(dataMatrix$dataMatrix)))
  
  idxNodes <- 1:(nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))
  idxNodesUp <- (nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))
  idxNodesDown <- (2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):(3*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))
  
  # edges
  edgesUp <- paste0("xb", (length(nodesALL)+1):(length(nodesALL)+nrow(pknList)*nrow(dataMatrix$dataMatrix)))
  edgesDown <- paste0("xb", (length(nodesALL)+nrow(pknList)*nrow(dataMatrix$dataMatrix)+1):(length(nodesALL)+nrow(pknList)*nrow(dataMatrix$dataMatrix)+nrow(pknList)*nrow(dataMatrix$dataMatrix)))
  
  expEdgesUp <- c()
  expEdgesDown <- c()
  expEdgesReducedSource <- c()
  expEdgesReducedTarget <- c()
  idxExperimentEdges <- c()
  expNodesReducedUp <- c()
  expNodesReducedDown <- c()
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    expEdgesUp <- c(expEdgesUp, paste0("ReactionUp ", as.character(pknList$X1), "=", as.character(pknList$X3), " in experiment ", i))
    expEdgesDown <- c(expEdgesDown, paste0("ReactionDown ", as.character(pknList$X1), "=", as.character(pknList$X3), " in experiment ", i))
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
write_boundaries <- function(variables=variables){
  
  bounds <- paste0("\t", "-1 <= ", variables$variables[variables$idxNodes], " <= 1")
  bounds <- c(bounds, paste0("\t", "0 <= ", variables$variables[variables$idxNodesUp], " <= 1"))
  bounds <- c(bounds, paste0("\t", "0 <= ", variables$variables[variables$idxNodesDown], " <= 1"))
  bounds <- c(bounds, paste0("\t", "0 <= ", variables$variables[variables$idxEdgesUp], " <= 1"))
  bounds <- c(bounds, paste0("\t", "0 <= ", variables$variables[variables$idxEdgesDown], " <= 1"))
  
  return(bounds)
  
}

##
write_objective_function <- function(dataMatrix = dataMatrix, variables = variables, alpha=alpha, beta=beta){
  
  measured <- gsub(colnames(dataMatrix$dataMatrix)[dataMatrix$dsID], pattern = "DS:", replacement = "")
  
  idxMeasured <- c()
  for(i in 1:length(measured)){
    
    # idxMeasured <- c(idxMeasured, which(variables$expNodesReduced==paste0("Species ", measured[i])))
    
    idxMeasured <- which(variables$expNodesReduced==paste0("Species ", measured[i]))
    
  }
  
  measuredVar <- variables$variables[idxMeasured]
  
  
  objectiveFunctionVec <- paste0(" + ", alpha, " absDiff", gsub(measuredVar, pattern = "xb", replacement = ""))
  objectiveFunction <- paste(objectiveFunctionVec, collapse = "")
  
  objectiveFunction <- substring(text = objectiveFunction[1], first = 4, last = nchar(objectiveFunction))
  
  objectiveFunction <- paste0("obj:\t", objectiveFunction)
  
  objectiveFunctionUpVec <- paste0(" + ", beta, " ", variables$variables[variables$idxNodesUp])
  objectiveFunctionUp <- paste(objectiveFunctionUpVec, collapse = "")
  
  objectiveFunctionDownVec <- paste0(" + ", beta, " ", variables$variables[variables$idxNodesDown])
  objectiveFunctionDown <- paste(objectiveFunctionDownVec, collapse = "")
  
  objectiveFunction <- paste0(objectiveFunction, objectiveFunctionUp)
  objectiveFunction <- paste0(objectiveFunction, objectiveFunctionDown)
  
  return(objectiveFunction)
  
}

##
write_constraints_objFunction <- function(variables=variables, dataMatrix=dataMatrix){
  
  measurements <- as.vector(t(dataMatrix$dataMatrixSign))
  
  # idx1 <- which(measurements==0)
  idx2 <- which(measurements==1)
  idx3 <- which(measurements==-1)
  
  cc1 <- rep("", length(measurements))
  cc2 <- rep("", length(measurements))
  
  # cc1[idx1] <- paste0(variables$variables[idx1], " - absDiff", idx1, " <= 0")
  # cc2[idx1] <- paste0(variables$variables[idx1], " + absDiff", idx1, " >= 0")
  
  cc1[idx2] <- paste0(variables$variables[idx2], " - absDiff", idx2, " <= 1")
  cc2[idx2] <- paste0(variables$variables[idx2], " + absDiff", idx2, " >= 1")
  
  cc1[idx3] <- paste0(variables$variables[idx3], " - absDiff", idx3, " <= -1")
  cc2[idx3] <- paste0(variables$variables[idx3], " + absDiff", idx3, " >= -1")
  
  constraints0 <- c(cc1, cc2)
  
  return(constraints0[-which(constraints0=="")])
}

##
write_constraints_1 <- function(variables=variables){
  
  constraints1 <- rep("", length(variables$idxEdgesUp))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints1[idx1] <- paste0(variables$variables[variables$idxEdgesUp[idx1]], " - ",
                     variables$variables[which(paste0("Species ", variables$reactionSource[idx1])==variables$expNodesReduced)], " >= 0")
  
  constraints1[idx2] <- paste0(variables$variables[variables$idxEdgesUp[idx2]], " + ",
                     variables$variables[which(paste0("Species ", variables$reactionSource[idx2])==variables$expNodesReduced)], " >= 0")
  
  return(constraints1)
  
}

##
write_constraints_2 <- function(variables=variables){
  
  constraints2 <- rep("", length(variables$idxEdgesDown))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints2[idx1] <- paste0(variables$variables[variables$idxEdgesDown[idx1]], " + ",
                               variables$variables[which(paste0("Species ", variables$reactionSource[idx1])==variables$expNodesReduced)], " >= 0")
  
  constraints2[idx2] <- paste0(variables$variables[variables$idxEdgesDown[idx2]], " - ",
                               variables$variables[which(paste0("Species ", variables$reactionSource[idx2])==variables$expNodesReduced)], " >= 0")
  
  return(constraints2)
  
}

##
write_constraints_3 <- function(variables=variables){
  
  constraints3 <- paste0(variables$variables[variables$idxEdgesUp], " + ", variables$variables[variables$idxEdgesDown], " <= 1")
  
  return(constraints3)
  
}

##
write_constraints_4 <- function(variables=variables){
  
  constraints4 <- rep("", length(variables$idxEdgesUp))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints4[idx1] <- paste0(variables$variables[variables$idxEdgesUp[idx1]], " - ",
                               variables$variables[which(paste0("Species ", variables$reactionSource[idx1])==variables$expNodesReduced)],
                               " - ", variables$variables[variables$idxEdgesDown[idx1]], " <= 0")
  
  constraints4[idx2] <- paste0(variables$variables[variables$idxEdgesUp[idx2]], " + ",
                               variables$variables[which(paste0("Species ", variables$reactionSource[idx2])==variables$expNodesReduced)],
                               " - ", variables$variables[variables$idxEdgesDown[idx2]], " <= 0")
  
  return(constraints4)
  
}

##
write_constraints_5 <- function(variables=variables){
  
  constraints5 <- rep("", length(variables$idxEdgesDown))
  
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)
  
  constraints5[idx1] <- paste0(variables$variables[variables$idxEdgesDown[idx1]], " + ",
                               variables$variables[which(paste0("Species ", variables$reactionSource[idx1])==variables$expNodesReduced)],
                               " - ", variables$variables[variables$idxEdgesUp[idx1]], " <= 0")
  
  constraints5[idx2] <- paste0(variables$variables[variables$idxEdgesDown[idx2]], " - ",
                               variables$variables[which(paste0("Species ", variables$reactionSource[idx2])==variables$expNodesReduced)],
                               " - ", variables$variables[variables$idxEdgesUp[idx2]], " <= 0")
  
  return(constraints5)
  
}

##
write_constraints_6 <- function(variables=variables, dataMatrix=dataMatrix){
  
  constraints6 <- c()
  
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    source <- unique(variables$reactionSource)
    target <- unique(variables$reactionTarget)
    
    for(ii in 1:length(target)){
      
      pp1 <- variables$variables[which(variables$exp==paste0("SpeciesUP ", target[ii], " in experiment ", i))]
      
      for(jj in 1:length(source)){
        
        reaction <- paste0("ReactionUp ", source[jj], "=", target[ii], " in experiment ", i)
        
        if(length(which(variables$exp==reaction)) > 0){
          
          for(kk in 1:length(which(variables$exp==reaction))){
            
            pp2 <- paste0(" - ", variables$variables[which(variables$exp==reaction)[kk]])
            
          }
          
          constraints6 <- c(constraints6, paste0(pp1, pp2, " <= 0"))
          
        }
        
      }
      
    }
    
  }
  
  return(constraints6)
  
}

##
write_constraints_7 <- function(variables=variables, dataMatrix=dataMatrix){
  
  constraints6 <- c()
  
  for(i in 1:nrow(dataMatrix$dataMatrix)){
    
    source <- unique(variables$reactionSource)
    target <- unique(variables$reactionTarget)
    
    for(ii in 1:length(target)){
      
      pp1 <- variables$variables[which(variables$exp==paste0("SpeciesDown ", target[ii], " in experiment ", i))]
      
      for(jj in 1:length(source)){
        
        reaction <- paste0("ReactionDown ", source[jj], "=", target[ii], " in experiment ", i)
        
        if(length(which(variables$exp==reaction)) > 0){
          
          for(kk in 1:length(which(variables$exp==reaction))){
            
            pp2 <- paste0(" - ", variables$variables[which(variables$exp==reaction)[kk]])
            
          }
          
          constraints6 <- c(constraints6, paste0(pp1, pp2, " <= 0"))
          
        }
        
      }
      
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
  
  # inputs <- as.vector(t(inputs))
  
  inputsZero <- which(inputs==0, arr.ind = TRUE)
  inputsUp <- which(inputs==1, arr.ind = TRUE)
  inputsDown <- which(inputs==-1, arr.ind = TRUE)
  
  # ii <- as.vector(t(inputs))
  
  constraints8 <- paste0(variables$variables[variables$idxNodesUp], " - ",
                         variables$variables[variables$idxNodesDown], " - ",
                         variables$variables[variables$idxNodes], " = 0")
  
  if(nrow(inputsUp) > 0){
    
    for(i in 1:nrow(inputsUp)){
      
      ss1 <- variables$variables[which(variables$exp==paste0("SpeciesUP ", colnames(inputs)[inputsUp[i, 2]], " in experiment ", inputsUp[i, 1]))]
      
      ss2 <- variables$variables[which(variables$exp==paste0("SpeciesDown ", colnames(inputs)[inputsUp[i, 2]], " in experiment ", inputsUp[i, 1]))]
      
      ss3 <- variables$variables[which(variables$exp==paste0("Species ", colnames(inputs)[inputsUp[i, 2]], " in experiment ", inputsUp[i, 1]))]
      
      idx <- which(constraints8==paste0(ss1, " - ", ss2, " - ", ss3, " = 0"))
      
      constraints8[idx] <- gsub(constraints8[idx], pattern = " = 0", replacement = " = -1")
      
    }
    
  }
  
  if(nrow(inputsDown) > 0){
    
    for(i in 1:nrow(inputsDown)){
      
      ss1 <- variables$variables[which(variables$exp==paste0("SpeciesUP ", colnames(inputs)[inputsDown[i, 2]], " in experiment ", inputsDown[i, 1]))]
      
      ss2 <- variables$variables[which(variables$exp==paste0("SpeciesDown ", colnames(inputs)[inputsDown[i, 2]], " in experiment ", inputsDown[i, 1]))]
      
      ss3 <- variables$variables[which(variables$exp==paste0("Species ", colnames(inputs)[inputsDown[i, 2]], " in experiment ", inputsDown[i, 1]))]
      
      idx <- which(constraints8==paste0(ss1, " - ", ss2, " - ", ss3, " = 0"))
      
      constraints8[idx] <- gsub(constraints8[idx], pattern = " = 0", replacement = " = 1")
      
    }
    
  }
  
  return(constraints8)
  
}

##
all_constraints <- function(c0=c0, c1=c1, c2=c2, c3=c3, c4=c4, c5=c5, c6=c6, c7=c7, c8=c8){
  
  allConst <- c(c0, c1, c2, c3, c4, c5, c6, c7, c8)
  
  allConstraints <- paste0("c", 1:length(allConst), ":\t", allConst, "\t \t")
  
  return(allConstraints)
  
}










