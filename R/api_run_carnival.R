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
#'appears across all solutions.
#'
#'2. nodesAttributes: A table with 6 columns containing information about
#'infered protein activity states and attributes. It contains the Protein IDs
#'(Node); how often this node has taken an activity of 0, 1 and -1 across the
#'solutions (ZeroAct, UpAct, DownAct); the average activities across solutions
#'(AvgAct); and the node attribute (measured, target, inferred).
#'
#'3. sifAll: A list of separate network solutions.
#'
#'4. attributesAll: A list of separate inferred node activities in each
#' solution.
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
#' res1 = runCarnival(perturbations = toy_inputs_ex1, 
#'                    measurements = toy_measurements_ex1,
#'                    priorKnowledgeNetwork = toy_network_ex1)
#'
#' ## cbc
#' res2 = runCarnival(perturbations = toy_inputs_ex1, 
#'                    measurements = toy_measurements_ex1,
#'                    priorKnowledgeNetwork = toy_network_ex1, 
#'                    solver = supportedSolvers$cbc)
#'
#' ## cplex
#' res3 = runCarnival(perturbations = toy_inputs_ex1, 
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

prerunCarnival <- function(perturbations, 
                           measurements, 
                           priorKnowledgeNetwork, 
                           pathwayWeights = NULL,
                           solver = supportedSolvers$lpSolve,
                           solverPath = "",
                           newDataRepresentation = F, #will be removed in the next version
                           carnivalOptions = 
                             defaultLpSolveCarnivalOptions()) {
  message("--- Start of the CARNIVAL pipeline ---")
  message("Carnival flavour: prerun") 
  
  dataPreprocessed <- checkData( perturbations = perturbations, 
                                 measurements = measurements, 
                                 priorKnowledgeNetwork = priorKnowledgeNetwork,
                                 pathwayWeights = pathwayWeights )
  
  checkSolverInputs(carnivalOptions)
  carnivalOptions <- collectMetaInfo(carnivalOptions)
  
  results <- prepareForCarnivalRun (dataPreprocessed = dataPreprocessed,
                                    carnivalOptions = carnivalOptions, 
                                    newDataRepresentation)
  
  cleanupCarnival(carnivalOptions)
  message(" ") 
  message("--- End of the CARNIVAL pipeline --- ")
  message(" ")
  
  return(results)
}

runCarnival <- function( perturbations, 
                         measurements, 
                         priorKnowledgeNetwork, 
                         pathwayWeights = NULL,
                         solver = supportedSolvers$lpSolve,
                         solverPath = "",
                         newDataRepresentation = F, #will be removed in the next version
                         carnivalOptions = 
                           defaultLpSolveCarnivalOptions()) {
  
  message("--- Start of the CARNIVAL pipeline ---")
  message("Carnival flavour: vanilla") 
  
  dataPreprocessed <- checkData( perturbations = perturbations, 
                                 measurements = measurements, 
                                 priorKnowledgeNetwork = priorKnowledgeNetwork,
                                 pathwayWeights = pathwayWeights )
  
  checkSolverInputs(carnivalOptions)
  carnivalOptions <- collectMetaInfo(carnivalOptions)
  
  result <- solveCarnival( dataPreprocessed,
                           carnivalOptions, 
                           newDataRepresentation )  
  cleanupCarnival(carnivalOptions)

  message(" ") 
  message("--- End of the CARNIVAL pipeline --- ")
  message(" ")
  
  return(result)
}

runCarnivalFromLp <- function(lpFile = "",
                              parsedDataFile = "",
                              solver = supportedSolvers$lpSolve,
                              solverPath = "",
                              carnivalOptions = 
                                defaultLpSolveCarnivalOptions()) {
  
  message("--- Start of the CARNIVAL pipeline ---")
  message("Carnival flavour: vanilla")  
  checkSolverInputs(carnivalOptions)
  carnivalOptions <- collectMetaInfo(carnivalOptions)
  
  result <- solveCarnivalFromLp( lpFile = "",
                                 parsedDataFile = "",
                                 carnivalOptions = carnivalOptions )
  cleanupCarnival(carnivalOptions)
  
  return(result)
}

runInverseCarnival <- function(measurements, 
                               priorKnowledgeNetwork, 
                               pathwayWeights = NULL,
                               solverPath = solverPath,
                               solver = supportedSolvers$lpSolve,
                               carnivalOptions = 
                                 defaultCplexCarnivalOptions(solverPath = solverPath)()){
  message(" ") 
  message("--- Start of the CARNIVAL pipeline ---")
  message("Carnival flavour: inverse") 
  
  dataPreprocessed <- checkData( perturbations = perturbations, 
                                 measurements = measurements, 
                                 priorKnowledgeNetwork = priorKnowledgeNetwork,
                                 pathwayWeights = pathwayWeights )
  
  checkSolverInputs(carnivalOptions)
  carnivalOptions <- collectMetaInfo(carnivalOptions)
  
  result <- solveCarnival( dataPreprocessed, carnivalOptions )
  cleanupCarnival(carnivalOptions)
  
  message(" ") 
  message("--- End of the CARNIVAL pipeline --- ")
  message(" ")
  
  return(result)
}

runCarnivalWithManualConstraints <- function(perturbations, 
                                             measurements, 
                                             priorKnowledgeNetwork, 
                                             pathwayWeights = NULL,
                                             solver = supportedSolvers$lpSolve,
                                             solverPath = "",
                                             constraints = c(),
                                             carnivalOptions = 
                                               defaultLpSolveCarnivalOptions()) {
  stop("Function is not implemented yet.")
  return(NULL)
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


