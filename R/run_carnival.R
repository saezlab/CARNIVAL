#'\code{runCARNIVAL}
#'
#'@details Run CARNIVAL pipeline using to the user-provided list of inputs or
#'run CARNIVAL built-in examples
#'
#'@param perturbations Vector of targets of perturbation - optional
#'or default set to NULL to run invCARNIVAL when inputs are not known.
#'@param measurements Vector of the measurements (i.e. DoRothEA normalised
#'enrichment scores) - always required.
#'@param priorKnowledgeNetwork Data frame of the prior knowledge network - always required.
#'@param pathwayWeights Vector of the additional weight (i.e. PROGENy pathway
#'score or measured protein activities) - optional or default set as NULL to run
#'CARNIVAL without weights.

#'@return The function will return a list of results containing:
#'1. weightedSIF: A table with 4 columns containing the combined network
#'solutions from CARNIVAL. It contains the Source of the interaction (Node1),
#'Sign of the interaction (Sign), the Target of the interaction (Node2) and the
#'weight of the interaction (Weight) which shows how often an interaction
#'appears across the various solutions.
#'
#'2. nodesAttributes: A table with 6 columns containing information about
#'infered protein activity states and attributes. It contains the Protein IDs
#'(Node); how often this node has taken an activity of 0, 1 and -1 across the
#'solutions (ZeroAct, UpAct, DownAct); the average activities across solutions
#'(AvgAct); and the node attribute (measured, target, inferred).
#'
#'3. sifAll: A list of separate network solutions
#'
#'4. attributesAll: A list of separate inferred node activities across the
#'various solutions.
#'
#'5. diagnostics: reports the convergence of optimization and reason of 
#' the termination. Only for CPLEX solver. 
#'
#'@author Enio Gjerga, 2020 \email{carnival.developers@gmail.com}
#'
#'@examples
#' load(file = system.file("toy_perturbations_ex1.RData",
#'                         package="CARNIVAL"))
#' load(file = system.file("toy_measurements_ex1.RData",
#'                         package="CARNIVAL"))
#' load(file = system.file("toy_network_ex1.RData",
#'                         package="CARNIVAL"))
#'
#' ## lpSolve
#' res1 = runCARNIVAL(perturbations = toy_inputs_ex1, 
#'                    measurements = toy_measurements_ex1,
#'                    priorKnowledgeNetwork = toy_network_ex1)
#'
#' ## cbc
#' res2 = runCARNIVAL(perturbations = toy_inputs_ex1, 
#'                    measurements = toy_measurements_ex1,
#'                    priorKnowledgeNetwork = toy_network_ex1, 
#'                    solver = supportedSolvers$cbc)
#'
#' ## cplex
#' res3 = runCARNIVAL(perturbations = toy_inputs_ex1, 
#'                    measurements = toy_measurements_ex1,
#'                    priorKnowledgeNetwork = toy_network_ex1, 
#'                    solver = supportedSolver$cplex,
#'                    solverPath = "your_path/")
#'
#'@import doParallel
#'@import readr
#'@import readxl
#'@import lpSolve
#'@import igraph
#'
#'@export
#'

#TODO make it possible to change just one/two params from here
#TODO change default option to lpSolve later in carnival options
#TODO make a data structure containing all data in one
runCarnival <- function( perturbations, 
                         measurements, 
                         priorKnowledgeNetwork, 
                         pathwayWeights = NULL,
                         solver = supportedSolvers$lpSolve,
                         solverPath = "",
                         carnivalOptions = 
                           defaultCplexCarnivalOptions(solverPath = solverPath) ) {


  resultDataCheck <- checkData( perturbations = perturbations, 
                                measurements = measurements, 
                                priorKnowledgeNetwork = priorKnowledgeNetwork,
                                pathwayWeights = pathwayWeights )
  
  resultOptionsCheck <- checkSolverInputs(carnivalOptions)
  resultsChecks <- c(resultDataCheck, resultOptionsCheck)
  print(resultDataCheck)
  
  if (carnivalOptions$cleanTmpFiles) {
    cleanupCARNIVAL(carnivalOptions$keepLPFiles)
  }

  result <- solveCarnivalSingleRun( perturbations = resultsChecks$perturbations,
                                    measurements = resultsChecks$measurements,
                                    priorKnowledgeNetwork = resultsChecks$priorKnowledgeNetwork,
                                    pathwayWeights = resultsChecks$weights,
                                    carnivalOptions = carnivalOptions )

  if (carnivalOptions$cleanTmpFiles) {
    cleanupCARNIVAL(carnivalOptions$keepLPFiles)
  }
  
  return(result)
}

#TODO 
run_inverse_carnival <- function(measurements, 
                                 priorKnowledgeNetwork, 
                                 pathwayWeights,
                                 carnivalOptions = default_carnival_options()){
  
}

#TODO
runCarnivalWithManualConstraints <- function() {
  
}

#For backward compatibility with previous API
runCARNIVAL <- function(inputObj=NULL,
                        measObj=measObj,
                        netObj=netObj,
                        weightObj=NULL,
                        solverPath=NULL,
                        solver=c('lpSolve', 'cplex', 'cbc'),
                        timelimit=3600,
                        mipGAP=0.05,
                        poolrelGAP=0.0001,
                        limitPop=500,
                        poolCap=100,
                        poolIntensity=4,
                        poolReplace=2,
                        alphaWeight=1,
                        betaWeight=0.2,
                        threads=0,
                        cplexMemoryLimit=8192,
                        cleanTmpFiles=TRUE,
                        dir_name=NULL) {
  
  solver <- match.arg(solver)
 
  opts = c(solverPath = solverPath,
           solver = solver, 
           timelimit = timelimit,
           mipGap = mipGAP,
           poolrelGap = poolrelGAP,
           limitPop = limitPop, 
           poolCap = poolCap,
           poolIntensity = poolIntensity,
           poolReplace = poolReplace,
           alphaWeight = alphaWeight, 
           betaWeight = betaWeight,
           threads = threads,
           cplexMemoryLimit = cplexMemoryLimit,
           cleanTmpFiles = cleanTmpFiles,
           dirName = dir_name)
  
  result <- runCarnival(perturbations = inputObj, 
                        measurements = measObj, 
                        priorKnowledgeNetwork = netObj, 
                        pathwayWeights = weightObj, 
                        carnivalOptions = opts)
  
  return(result)
  
}
