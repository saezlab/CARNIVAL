#'\code{runCARNIVAL}
#'
#'@details Run CARNIVAL pipeline using to the user-provided list of inputs or
#'run CARNIVAL built-in examples
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
#'@param DOTfig For plotting: define if DOT figure will be exported in the
#'result folder (logical TRUE/FALSE). Default set to TRUE. If this the case, the
#'plotted DOT figures will be saved in the defined dir_name directory.
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
#'@param dir_name Specify directory name to store results. by default it will
#'create a /DOTfigures directory within the current working directory
#'
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
#'@author Enio Gjerga, 2020 \email{carnival.developers@gmail.com}
#'
#'@examples
#' load(file = system.file("toy_inputs_ex1.RData",
#'                         package="CARNIVAL"))
#' load(file = system.file("toy_measurements_ex1.RData",
#'                         package="CARNIVAL"))
#' load(file = system.file("toy_network_ex1.RData",
#'                         package="CARNIVAL"))
#'
#' ## lpSolve
#' res1 = runCARNIVAL(inputObj = toy_inputs_ex1, measObj = toy_measurements_ex1,
#'                    netObj = toy_network_ex1)
#'
#' ## cbc
#' res2 = runCARNIVAL(inputObj = toy_inputs_ex1, measObj = toy_measurements_ex1,
#'                    netObj = toy_network_ex1, solverPath = solverPath,
#'                    solver = "cbc")
#'
#' ## cplex
#' res3 = runCARNIVAL(inputObj = toy_inputs_ex1, measObj = toy_measurements_ex1,
#'                    netObj = toy_network_ex1, solverPath = solverPath,
#'                    solver = "cbc")
#'
#'@import doParallel
#'@import readr
#'@import readxl
#'@import lpSolve
#'@import igraph
#'
#'@export
#'

runCARNIVAL <- function(inputObj=NULL,
                        measObj=measObj,
                        netObj=netObj,
                        weightObj=NULL,
                        solverPath=NULL,
                        solver="lpSolve",
                        DOTfig=FALSE,
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
                        dir_name=paste0(getwd(), "/DOTfigures"))
{

  res = checkInputs(solverPath = solverPath, netObj = netObj, measObj = measObj,
                    inputObj = inputObj, weightObj = weightObj, DOTfig = DOTfig,
                    timelimit = timelimit, mipGAP = mipGAP,
                    poolrelGAP = poolrelGAP, limitPop = limitPop,
                    poolCap = poolCap, poolIntensity = poolIntensity,
                    poolReplace = poolReplace, alphaWeight = alphaWeight,
                    betaWeight = betaWeight, dir_name = dir_name,
                    solver = solver, threads = threads)

  cleanupCARNIVAL(condition = res$condition, repIndex = res$repIndex)

  result = solveCARNIVAL(solverPath = solverPath, netObj = res$inputs$network,
                         measObj = res$measurements,
                         inputObj = res$inputs$inputs,
                         weightObj = res$weights, DOTfig = DOTfig,
                         timelimit = timelimit, mipGAP = mipGAP,
                         poolrelGAP = poolrelGAP, limitPop = limitPop,
                         poolCap = poolCap, poolIntensity = poolIntensity,
                         poolReplace = poolReplace, alphaWeight = alphaWeight,
                         betaWeight = betaWeight, dir_name = dir_name,
                         solver = solver,
                         threads = threads,
                         experimental_conditions = res$exp,
                         condition = res$condition, repIndex = res$repIndex)

  cleanupCARNIVAL(condition = res$condition, repIndex = res$repIndex)

  return(result)

}
