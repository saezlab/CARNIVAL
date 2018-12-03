#'\code{create_variables_all}
#'
#'@param pknList Contains the background network which serves as a prior knowledge and which we train.
#'@param dataMatrix Contains the matrix which stores the data for running CARNIVAL and a set of identifiers for Targets, Measured and Un-measured nodes.
#'@return This function returns the identifiers of all the variables used in the ILP formulation together with an explanation about the meaning of each of them. Also it returns a list of useful identifiers.

create_variables_all <- function(pknList=pknList, dataMatrix=dataMatrix){

  # ======================================= #
  # ======= Load create_variables.R ======= #
  # ======================================= #

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

    ##
    #Introducing distance variables

    dist <- paste0("dist_", sapply(strsplit(expNodesALL[idxNodes], split = " "), "[[", 2))

    distExp <- paste0("Distance ", sapply(strsplit(expNodesALL[idxNodes], split = " "), "[[", 2))

    ##
    #Introducing B variables

    varB <- paste0("B_", sapply(strsplit(expNodes, split = " "),function(x) x[2]), "_", conditionIDX)
    expVarB <- paste0("B variable for ", sapply(strsplit(expNodes, split = " "),function(x) x[2]), " in experiment ", conditionIDX)

    ##
    #Matching table for u variables
    uTable <- matrix(data = , nrow = length(idxEdgesUp), ncol = 2)
    uTable[, 1] <- c(nodesALL, edgesALL, varB, dist)[idxEdgesUp]
    uTable[, 2] <- c(nodesALL, edgesALL, varB, dist)[idxEdgesDown]


    # output
    res <- list(variables=c(nodesALL, edgesALL, varB, dist), exp=c(expNodesALL, expEdgesALL, expVarB, distExp), idxNodes=idxNodes, idxNodesUp=idxNodesUp,
                idxNodesDown=idxNodesDown, idxEdgesUp=idxEdgesUp, idxEdgesDown=idxEdgesDown, signs=signs,
                reactionSource=reactionSource, reactionTarget=reactionTarget, expNodesReduced=expNodesReduced,
                expNodesReducedUpSource=expNodesReducedUpSource, expNodesReducedDownSource=expNodesReducedDownSource,
                expNodesReducedDownTarget=expNodesReducedDownTarget, expNodesReducedUpTarget=expNodesReducedUpTarget,
                expEdgesReducedSource=expEdgesReducedSource, expEdgesReducedTarget=expEdgesReducedTarget,
                idxExperimentNodes=idxExperimentNodes, idxExperimentEdges=idxExperimentEdges,
                expNodesReducedUp=expNodesReducedUp, expNodesReducedDown=expNodesReducedDown, idxB = (length(c(nodesALL, edgesALL))+1):(length(c(nodesALL, edgesALL))+length(varB)),
                idxDist = (length(c(nodesALL, edgesALL))+length(varB)+1):(length(c(nodesALL, edgesALL))+length(varB)+length(dist)), uTable = uTable)

    return(res)

  }

  # ====================================== #
  # ====================================== #
  # ====================================== #

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
