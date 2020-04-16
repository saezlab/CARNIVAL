## This function returns the identifiers of all the variables used in the ILP 
## formulation together with an explanation about the meaning of each of them. 
## Also it returns a list of useful identifiers.
##
## Enio Gjerga, 2020

create_variables_all <- function(pknList=pknList, dataMatrix=dataMatrix){

  ## ======================================= ##
  ## ======= Load create_variables.R ======= ##
  ## ======================================= ##

  create_variables <- function(pknList=pknList, dataMatrix = dataMatrix, 
                               conditionIDX=conditionIDX){

    colnames(pknList) <- c("X1", "X2", "X3")

    nodes <- paste0("xb", seq_len((nrow(dataMatrix$dataMatrix)*
                                     ncol(dataMatrix$dataMatrix))), 
                    "_", conditionIDX)
    nodesUp <- paste0("xb", seq(from = nrow(dataMatrix$dataMatrix)*
                                  ncol(dataMatrix$dataMatrix)+1, 
                                to = 2*nrow(dataMatrix$dataMatrix)*
                                  ncol(dataMatrix$dataMatrix), 
                                by = 1), 
                      "_", conditionIDX)
    nodesDown <- paste0("xb", seq(from = 2*nrow(dataMatrix$dataMatrix)*
                                    ncol(dataMatrix$dataMatrix)+1, 
                                  to = 3*nrow(dataMatrix$dataMatrix)*
                                    ncol(dataMatrix$dataMatrix), by = 1), 
                        "_", conditionIDX)

    expNodes <- paste0("Species ", dataMatrix$species, 
                       " in experiment ", conditionIDX)
    expNodesReduced = paste0("Species ", 
                             dataMatrix$species)
    expNodesUp <- paste0("SpeciesUP ", dataMatrix$species, 
                         " in experiment ", conditionIDX)
    expNodesDown <- paste0("SpeciesDown ", dataMatrix$species, 
                           " in experiment ", conditionIDX)
    expNodesReducedUpSource <- as.character(pknList$X1)
    
    expNodesReducedDownSource <- as.character(pknList$X1)
    
    expNodesReducedUpTarget <- as.character(pknList$X3)
    
    expNodesReducedDownTarget <- as.character(pknList$X3)
    
    idxExperimentNodes <- length(expNodes)

    nodesALL <- c(nodes, nodesUp, nodesDown)
    expNodesALL <- c(expNodes, expNodesUp, expNodesDown)

    idxNodes <- seq_len(nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))
    idxNodesUp <- seq(from = nrow(dataMatrix$dataMatrix)*
                        ncol(dataMatrix$dataMatrix)+1,
                      to = 2*nrow(dataMatrix$dataMatrix)*
                        ncol(dataMatrix$dataMatrix),
                      by = 1)
    idxNodesDown <- seq(from = 2*nrow(dataMatrix$dataMatrix)*
                          ncol(dataMatrix$dataMatrix)+1, 
                        to = 3*nrow(dataMatrix$dataMatrix)*
                          ncol(dataMatrix$dataMatrix), 
                        by = 1)

    # edges
    edgesUp <- paste0("xb", seq(from = length(nodesALL)+1, 
                                to = length(nodesALL)+
                                  nrow(pknList)*
                                  nrow(dataMatrix$dataMatrix), by = 1), 
                      "_", conditionIDX)
    
    edgesDown <- paste0("xb", seq(from = length(nodesALL)+
                                    nrow(pknList)*
                                    nrow(dataMatrix$dataMatrix)+1, 
                                  to = length(nodesALL)+
                                    nrow(pknList)*
                                    nrow(dataMatrix$dataMatrix)
                                  +nrow(pknList)*
                                    nrow(dataMatrix$dataMatrix), by = 1), 
                        "_", conditionIDX)

    expEdgesUp <- paste0("ReactionUp ", as.character(pknList$X1), "=", 
                                       as.character(pknList$X3), 
                                       " in experiment ", conditionIDX)
    
    expEdgesDown <- paste0("ReactionDown ", as.character(pknList$X1), "=", 
                                           as.character(pknList$X3), 
                                           " in experiment ", conditionIDX)
    
    expEdgesReducedSource <- paste0("ReactionSource ", 
                                      as.character(pknList$X1))
    
    expEdgesReducedTarget <- paste0("ReactionTarget ", 
                                      as.character(pknList$X3))
    
    expNodesReducedUp <- pknList$X1
    
    expNodesReducedDown <- pknList$X3
    
    idxExperimentEdges <- length(expEdgesUp)

    edgesALL <- c(edgesUp, edgesDown)
    expEdgesALL <- c(expEdgesUp, expEdgesDown)

    idxEdgesUp <- seq(from = length(nodesALL)+1, to = length(nodesALL)+
                        1+nrow(pknList)*nrow(dataMatrix$dataMatrix)-1, by = 1)
    
    idxEdgesDown <- seq(from = length(nodesALL)+
                          1+nrow(pknList)*nrow(dataMatrix$dataMatrix), 
                        to = length(nodesALL)+1+
                          nrow(pknList)*nrow(dataMatrix$dataMatrix)+
                          nrow(pknList)*nrow(dataMatrix$dataMatrix)-1, by = 1)

    signs <- pknList$X2
    reactionSource <- as.character(pknList$X1)
    reactionTarget <- as.character(pknList$X3)

    ##
    #Introducing distance variables

    dist <- paste0("dist_", sapply(strsplit(expNodesALL[idxNodes], 
                                            split = " "), "[[", 2))

    distExp <- paste0("Distance ", sapply(strsplit(expNodesALL[idxNodes],
                                                   split = " "), "[[", 2))

    ##
    #Introducing B variables

    varB <- paste0("B_", sapply(strsplit(expNodes, split = " "),
                                function(x) x[2]), "_", conditionIDX)
    expVarB <- paste0("B variable for ", sapply(strsplit(expNodes, split = " "),
                                                function(x) x[2]), 
                      " in experiment ", conditionIDX)

    ##
    #Matching table for u variables
    uTable <- matrix(data = , nrow = length(idxEdgesUp), ncol = 2)
    uTable[, 1] <- c(nodesALL, edgesALL, varB, dist)[idxEdgesUp]
    uTable[, 2] <- c(nodesALL, edgesALL, varB, dist)[idxEdgesDown]


    # output
    res <- list(variables=c(nodesALL, edgesALL, varB, dist), 
                exp=c(expNodesALL, expEdgesALL, expVarB, distExp), 
                idxNodes=idxNodes, idxNodesUp=idxNodesUp,
                idxNodesDown=idxNodesDown, idxEdgesUp=idxEdgesUp, 
                idxEdgesDown=idxEdgesDown, signs=signs,
                reactionSource=reactionSource, reactionTarget=reactionTarget, 
                expNodesReduced=expNodesReduced,
                expNodesReducedUpSource=expNodesReducedUpSource, 
                expNodesReducedDownSource=expNodesReducedDownSource,
                expNodesReducedDownTarget=expNodesReducedDownTarget, 
                expNodesReducedUpTarget=expNodesReducedUpTarget,
                expEdgesReducedSource=expEdgesReducedSource, 
                expEdgesReducedTarget=expEdgesReducedTarget,
                idxExperimentNodes=idxExperimentNodes, 
                idxExperimentEdges=idxExperimentEdges,
                expNodesReducedUp=expNodesReducedUp, 
                expNodesReducedDown=expNodesReducedDown, 
                idxB = seq(from = length(c(nodesALL, edgesALL))+1, 
                           to = length(c(nodesALL, edgesALL))+length(varB), 
                           by = 1),
                idxDist = seq(from = length(c(nodesALL, edgesALL))+
                                length(varB)+1, 
                              to = length(c(nodesALL, edgesALL))+
                                length(varB)+length(dist), 
                              by = 1), 
                uTable = uTable)

    return(res)

  }

  res <- list()
  ## namesRes <- c()

  for(i in seq_len(nrow(dataMatrix$dataMatrix))){

    dM <- dataMatrix
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))

    res[[length(res)+1]] <- create_variables(pknList = pknList, dataMatrix = dM, 
                                             conditionIDX = i)

    ## namesRes <- c(namesRes, paste0("Condition_", i))

  }

  names(res) <- paste0("Condition_", seq_len(nrow(dataMatrix$dataMatrix)))

  return(res)

}
