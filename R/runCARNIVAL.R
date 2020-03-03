#'\code{runCARNIVAL}
#'
#'Run CARNIVAL pipeline using to the user-provided list of inputs or run
#'CARNIVAL built-in examples
#'Note: The pipeline requires either all required user-defined input variables
#'(netObj and measObj) are set to NULL or CARNIVAL_example is set to NULL to
#'execute
#'
#'@param inputObj Data frame of the list for target of perturbation - optional
#'or default set to NULL to run invCARNIVAL when inputs are not known
#'@param measObj Data frame of the measurement file (i.e. DoRothEA normalised
#'enrichment scores) - always required
#'@param netObj Data frame of the prior knowledge network - always required
#'@param weightObj Data frame of the additional weight (i.e. PROGENy pathway
#'score or measured protein activities) - optional or default set as NULL to run
#'CARNIVAL without weights
#'@param solverPath Path to executable cbc/cplex file - default set to NULL, in
#'which case the solver from lpSolve package is used
#'@param solver Solver type that user wishes to use: lpSolve/cbc/cplex
#'(default set to lpSolve). The lpSolve does not require installation of any
#'solver and it can give only one solution and it is not optimal for modelling
#'of very large networks. The free cbc solver can be used for modelling of large
#'scale networks and it can also give only one solution. The cplex solver is
#'efficient, works well for lage scale networks and it can give multiple
#'solutions.
#'@param DOTfig For plotting: define if DOT figure will be exported in the
#'result folder (logical TRUE/FALSE - default set to FALSE)
#'@param timelimit CPLEX parameter: Time limit of CPLEX optimisation (in
#'seconds - default set to 600)
#'@param mipGAP CPLEX parameter: Allowed gap of accepted solution comparing to
#'the best solution (fraction; default: 0.05 = 5 percents)
#'@param poolrelGAP CPLEX parameter: Allowed relative gap of accepted solution
#'comparing within the pool of accepted solution (fraction; default: 0.0001)
#'@param inverseCR Execute the inverse CARNIVAL pipeline (logical TRUE/FALSE)
#'@param DOTfig For plotting: define if DOT figure will be exported in the
#'result folder (logical TRUE/FALSE)
#'@param timelimit CPLEX parameter: Time limit of CPLEX optimisation (in
#'seconds)
#'@param mipGAP CPLEX parameter: Allowed gap of accepted solution comparing to
#'the best solution (fraction; default: 0.05 = 5 percents)
#'@param poolrelGAP CPLEX parameter: Allowed relative gap of accepted solution
#'comparing within the pool of accepted solution (fraction; default: 0.0001)
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
#'@param dir_name Name of the directory where to store the DOT figure (by
#'default it will be stored in the /DOTfigures folder generated in the current
#'working directory).
#'@param parIdx Parallelization index (default set to NULL)
#'
#'@return The networks and predicted node activities from the CARNIVAL pipeline
#'as a variable which are also saved in the destined result folder
#'
#'@import doParallel
#'@import CARNIVAL
#'@import readr
#'@import readxl
#'@import lpSolve
#'
#'@export
#'
#'Enio Gjerga, 2020

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
                        dir_name=paste0(getwd(), "/DOTfigures"),
                        parIdx=NULL)
{

  res = checkInputs(solverPath = solverPath, netObj = netObj, measObj = measObj, 
                    inputObj = inputObj, weightObj = weightObj, DOTfig = DOTfig,
                    timelimit = timelimit, mipGAP = mipGAP, 
                    poolrelGAP = poolrelGAP, limitPop = limitPop, 
                    poolCap = poolCap, poolIntensity = poolIntensity, 
                    poolReplace = poolReplace, alphaWeight = alphaWeight, 
                    betaWeight = betaWeight, dir_name = dir_name, 
                    solver = solver, threads = threads, parIdx = parIdx)
  
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
