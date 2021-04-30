#'\code{generateLpFileCarnival}
#'
#'@details Prepare the input data for the run: tranforms data into lp file and .Rdata file.
#'These files can be reused to run CARNIVAL without preprocessing step using runCarnivalFromLp(..)
#'
#'@param perturbations (optional, if inverse CARNIVAL flavour is used further) vector of targets of perturbations.
#'@param measurements vector of the measurements (i.e. DoRothEA/VIPER normalised
#'enrichment scores)
#'@param priorKnowledgeNetwork data frame of the prior knowledge network
#'@param weights (optional) vector of the additional weights: e.g. PROGENy pathway
#'scores or measured protein activities.
#'@param solver ILP solver to use. Currently supported solvers can be obtained by getSupportedSolvers()
#'@param solverPath a path to an executable file of a solver (needed e.g. for cplex and cbc)
#'@param carnivalOptions the list of options for the run. See defaultLpSolveCarnivalOptions(), 
#'defaultLpSolveCarnivalOptions, defaultCbcCarnivalOptions.
#'
#'@return paths to .lp file and .RData file that can be used for runFromLpCarnival()
#'
#'@export
#'
generateLpFileCarnival <- function( perturbations = NULL,
                                    measurements,
                                    priorKnowledgeNetwork,
                                    weights = NULL,
                                    solver = getSupportedSolvers()$lpSolve,
                                    solverPath = "",
                                    carnivalOptions =
                                      defaultLpSolveCarnivalOptions()) {
  message("--- Start of the CARNIVAL pipeline ---")
  message(getTime(), " Carnival flavour: LP generator")

  dataPreprocessed <- checkData( perturbations, measurements,
                                 priorKnowledgeNetwork, weights )

  checkSolverInputs(carnivalOptions)
  carnivalOptions <- collectMetaInfo(carnivalOptions)

  variables <- prepareForCarnivalRun(dataPreprocessed, carnivalOptions)

  cleanupCarnival(carnivalOptions)
  
  message(getTime(), " All tasks finished.")
  message("\n", "--- End of the CARNIVAL pipeline --- ", "\n")

  return(c("lpFile" = carnivalOptions$filenames$lpFilename, 
           "parsedDataFile" = carnivalOptions$filenames$parsedData))
}

#'\code{runVanillaCarnival}
#'
#'@details Runs full CARNIVAL pipeline, vanilla(classic) flavour.
#'
#'@param perturbations vector of targets of perturbations.
#'@param measurements vector of the measurements (i.e. DoRothEA/VIPER normalised
#'enrichment scores)
#'@param priorKnowledgeNetwork data frame of the prior knowledge network
#'@param pathwayWeights (optional) vector of the additional weights: e.g. PROGENy pathway
#'score or measured protein activities.
#'@param solver ILP solver to use. Currently supported solvers can be obtained by getSupportedSolvers()
#'@param solverPath a path to an executable file of a solver (needed e.g. for cplex and cbc)
#'@param carnivalOptions the list of options for the run. See defaultLpSolveCarnivalOptions(), 
#'defaultLpSolveCarnivalOptions, defaultCbcCarnivalOptions.
#'
#'#'@return The function will return a list of results containing:
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
#'@author Enio Gjerga, Olga Ivanova 2020-2021 \email{carnival.developers@gmail.com}
#'
#'@export
runVanillaCarnival <- function( perturbations,
                                measurements,
                                priorKnowledgeNetwork,
                                weights = NULL,
                                solver = getSupportedSolvers()$lpSolve,
                                solverPath = "",
                                carnivalOptions =
                                  defaultLpSolveCarnivalOptions()) {

  message("--- Start of the CARNIVAL pipeline ---")
  message(getTime(), " Carnival flavour: vanilla")

  dataPreprocessed <- checkData( perturbations, measurements,
                                 priorKnowledgeNetwork, weights )

  checkSolverInputs(carnivalOptions)
  carnivalOptions <- collectMetaInfo(carnivalOptions)

  result <- solveCarnival(dataPreprocessed, carnivalOptions)
  cleanupCarnival(carnivalOptions)

  message(getTime(), " All tasks finished.")
  message("\n", "--- End of the CARNIVAL pipeline --- ", "\n")

  return(result)
}

#'\code{runCarnivalFromLp}
#'
#'@details Runs CARNIVAL pipeline with preparsed data - lp file and Rdata file containing variables for ILP formulation.
#' 
#'@param lpFile full path to .lp file
#'@param parsedDataFile full path to preprocessed .RData file
#'@param solver ILP solver to use. Currently supported solvers can be obtained by getSupportedSolvers()
#'@param solverPath a path to an executable file of a solver (needed e.g. for cplex and cbc)
#'@param carnivalOptions the list of options for the run. See defaultLpSolveCarnivalOptions(), 
#'defaultLpSolveCarnivalOptions, defaultCbcCarnivalOptions.
#'
# The function will return a list of results containing:
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
#'@author Enio Gjerga, Olga Ivanova 2020-2021 \email{carnival.developers@gmail.com}
#'
#'@export
runFromLpCarnival <- function(lpFile = "",
                              parsedDataFile = "",
                              solver = getSupportedSolvers()$lpSolve,
                              solverPath = "",
                              carnivalOptions =
                                defaultLpSolveCarnivalOptions()) {

  message("--- Start of the CARNIVAL pipeline ---")
  message(getTime(), " Carnival flavour: LP run")
  checkSolverInputs(carnivalOptions)
  carnivalOptions <- collectMetaInfo(carnivalOptions)

  result <- solveCarnivalFromLp( lpFile = lpFile,
                                 parsedDataFile = parsedDataFile,
                                 carnivalOptions = carnivalOptions )
  cleanupCarnival(carnivalOptions)
  
  message(getTime(), " All tasks finished.")
  message("\n", "--- End of the CARNIVAL pipeline --- ", "\n")
  
  return(result)
}

#'\code{runInverseCarnival}
#'
#'@details Runs CARNIVAL pipeline with preparsed data - lp file and Rdata file containing variables for ILP formulation.
#'
#'@param lpFile full path to .lp file
#'@param parsedDataFile full path to preprocessed .RData file
#'
# The function will return a list of results containing:
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
#'@author Enio Gjerga, Olga Ivanova 2020-2021 \email{carnival.developers@gmail.com}
#'
#'@export
runInverseCarnival <- function( measurements,
                                priorKnowledgeNetwork,
                                weights = NULL,
                                solverPath = "",
                                solver = getSupportedSolvers()$lpSolve,
                                carnivalOptions =
                                  defaultLpSolveCarnivalOptions() ){
  message(" ")
  message("--- Start of the CARNIVAL pipeline ---")
  message(getTime(), " Carnival flavour: inverse")

  dataPreprocessed <- checkData(  perturbations = NULL, 
                                  measurements, 
                                  priorKnowledgeNetwork, weights )

  checkSolverInputs(carnivalOptions)
  carnivalOptions <- collectMetaInfo(carnivalOptions)

  result <- solveCarnival( dataPreprocessed, carnivalOptions )
  cleanupCarnival(carnivalOptions)

  message(getTime(), " All tasks finished.")
  message("\n", "--- End of the CARNIVAL pipeline --- ", "\n")

  return(result)
}

runCarnivalWithManualConstraints <- function(perturbations,
                                             measurements,
                                             priorKnowledgeNetwork,
                                             pathwayWeights = NULL,
                                             solver = getSupportedSolvers()$lpSolve,
                                             solverPath = "",
                                             constraints = c(),
                                             carnivalOptions =
                                               defaultLpSolveCarnivalOptions()) {
  stop("Function is not implemented yet.")
  return(NULL)
}

#TODO examples! update the list of params - DF vs vectors
#'\code{runCARNIVAL}
#'
#'@details Run CARNIVAL pipeline using to the user-provided list of inputs or
#'run CARNIVAL built-in examples. The function is from v1.2 of CARNIVAL
#' and is left for backward compatibility. 
#'
#'@param inputObj Data frame of the list for target of perturbation - optional
#'or default set to NULL to run invCARNIVAL when inputs are not known.
#'@param measObj Data frame of the measurement file (i.e. DoRothEA normalised
#'enrichment scores) - always required.
#'@param netObj Data frame of the prior knowledge network - always required.
#'@param weightObj Data frame of the additional weight (i.e. PROGENy pathway
#'score or measured protein activities) - optional or default set as NULL to run
#'CARNIVAL without weights.
#'@param solverPath Path to executable cbc/cplex file - default set to NULL, in
#'which case the solver from lpSolve package is used.
#'@param solver Solver to use: lpSolve/cplex/cbc (Default set to lpSolve).
#'@param timelimit CPLEX/Cbc parameter: Time limit of CPLEX optimisation in
#'seconds (default set to 3600).
#'@param mipGAP CPLEX parameter: the absolute tolerance on the gap between the
#'best integer objective and the objective of the best node remaining. When this
#'difference falls below the value of this parameter, the linear integer
#'optimization is stopped (default set to 0.05)
#'@param poolrelGAP CPLEX/Cbc parameter: Allowed relative gap of accepted
#'solution comparing within the pool of accepted solution (default: 0.0001)
#'@param limitPop CPLEX parameter: Allowed number of solutions to be generated
#'(default: 500)
#'@param poolCap CPLEX parameter: Allowed number of solution to be kept in the
#'pool of solution (default: 100)
#'@param poolIntensity CPLEX parameter: Intensity of solution searching
#'(0,1,2,3,4 - default: 4)
#'@param alphaWeight Objective function: weight for mismatch penalty (default:
#'1 - will only be applied once measurement file only contains discrete values)
#'@param betaWeight Objective function: weight for node penalty (defaul: 0.2)
#'@param threads CPLEX parameter: Number of threads to use
#'default: 0 for maximum number possible threads on system
#'@param dir_name Specify directory name to store results. by default set to
#'NULL
#'

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
#'
#'@import readr
#'@import lpSolve
#'@import igraph
#'
#'@export
#'
runCARNIVAL <- function(inputObj = NULL,
                        measObj = measObj,
                        netObj = netObj,
                        weightObj  = NULL,
                        solverPath = NULL,
                        solver = c('lpSolve', 'cplex', 'cbc'),
                        timelimit = 3600,
                        mipGAP = 0.05,
                        poolrelGAP = 0.0001,
                        limitPop = 500,
                        poolCap = 100,
                        poolIntensity = 4,
                        poolReplace = 2,
                        alphaWeight = 1,
                        betaWeight = 0.2,
                        threads = 0,
                        cplexMemoryLimit = 8192,
                        cleanTmpFiles = TRUE,
                        dir_name = NULL) {
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
