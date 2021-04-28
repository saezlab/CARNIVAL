#' The list of supported solvers. 
supportedSolvers <- list(cplex = "cplex", cbc = "cbc", lpSolve = "lpSolve")
requiredCarnivalCplexOptions <- c("solverPath", "solver", "betaWeight")
requiredCplexOptions <- c("timelimit", "mipGap", "poolrelGap", 
                          "limitPop", "poolCap", "poolIntensity", 
                          "poolReplace", "threads")

#' default_CARNIVAL_options
#' 
#' generates default CARNIVAL options.  
#' 
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
#'@param betaWeight Objective function: weight for node penalty (defaul: 0.2)
#'@param threads CPLEX parameter: Number of threads to use
#'default: 0 for maximum number possible threads on system
#'@param dirName Specify directory name to store results. by default set to
#'NULL
#'
#' 
#' @return returns a list with all possible options implemented in CARNIVAL.
#' see the documentation on \code{\link{runCARNIVAL}}.
#' @export
#' 
defaultCplexCarnivalOptions <- function(solverPath=""){
    
    options <- list(
         solverPath = solverPath,
         solver = supportedSolvers$cplex, 
         lpFilename = "",
         cplexCommandFilename = "",
         outputFolder = "",
         betaWeight = 0.2,
         cleanTmpFiles = TRUE,
         keepLPFiles = TRUE,
         dirName = NULL
    )
    
    options <- c(options, suggestedCplexSpecificOptions())
    return(options)
}

defaultLpSolveCarnivalOptions <- function(){
  
  options <- list(
    solver = supportedSolvers$lpSolve, 
    lpFilename = "",
    outputFolder = "",
    betaWeight = 0.2,
    cleanTmpFiles = TRUE,
    keepLPFiles = TRUE
  )

  return(options)
}


defaultCbcSolveCarnivalOptions <- function(solverPath=""){
  options <- list(
    solver = supportedSolvers$cbc, 
    solverPath = solverPath,
    lpFilename = "",
    outputFolder = "",
    betaWeight = 0.2,
    cleanTmpFiles = TRUE,
    keepLPFiles = TRUE
  )
  
  options <- c(options, suggesteCbcSpecificOptions())
  return(options)
}

suggestedCplexSpecificOptions <- function() {
  
  options <- list(
    threads = 1,
    clonelog = -1,
    workdir = ".",
    mipGap = 0.05,
    timelimit = 3600, 
    poolrelGap = 0.0001,
    limitPop = 500,
    poolCap = 100,
    poolIntensity = 4,
    poolReplace = 2,
    cplexMemoryLimit=8192
  )
  
  return(options)
}

suggesteCbcSpecificOptions <- function() {
  options <- list(
    timelimit = 3600, 
    poolrelGap = 0.0001
  )
  return(options)
}


#TODO N.B. careful with scientific notation, it is switched off at another place in the code 
# (look up for scipen) - options(scipen = 0) to switch it on
defaultCplexSpecificOptions <- function() {
  options <- list(
        threads = 1,
        mipGap = 1e-04, 
        poolrelGap = 1e75,
        limitPop = 20,
        poolCap = 2.1e9,
        poolIntensity = 0,
        poolReplace = 0,
        timelimit = 1e+75
    )
    return(options)
}

readParameters <- function(jsonFileName = "parameters/carnival_cplex_parameters.json") {
  if ("rjson" %in% (.packages())) {
    message("Loading parameters file for CARNIVAL:", jsonFileName)
    parameters <- fromJSON(file = jsonFileName)  
    return(parameters)  
  } else {
    stop("Cannot read parameters from json: rjson package should be installed and loaded.")
  }
}


