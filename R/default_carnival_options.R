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
#'@param alphaWeight Objective function: weight for mismatch penalty (default:
#'1 - will only be applied once measurement file only contains discrete values)
#'@param betaWeight Objective function: weight for node penalty (defaul: 0.2)
#'@param threads CPLEX parameter: Number of threads to use
#'default: 0 for maximum number possible threads on system
#'@param dirName Specify directory name to store results. by default set to
#'NULL
#'
#' 
#' @return returns a list with all possible options implemented in CARNIVAL.
#' see the documentation on \code{\link{CARNIVAL::runCARNIVAL}}.
#' @export
#' 

supportedSolvers <- list(cplex="cplex", cbc="cbc", lpSolve="lpSolve")
requiredCarnivalCplexOptions <- c("solverPath", "solver", "alphaWeight", "betaWeight")
requiredCplexOptions <- c("timelimit", "mipGap", "poolrelGap", "limitPop", "poolCap", 
                          "poolIntensity", "poolReplace",
                          "threads")

defaultCplexCarnivalOptions <- function(solverPath=""){
    
    options <- list(
         solverPath=solverPath,
         solver=supportedSolvers$cplex, 
         timelimit=3600, 
         alphaWeight=1, 
         betaWeight=0.2,
         #TODO default value was 0 or 1?
         threads=1,
         cplexMemoryLimit=8192,
         cleanTmpFiles=TRUE,
         keepLPFiles=TRUE,
         dirName=NULL
    )
    
    options <- c(options, suggestedCplexOptions())
    return(options)
}

#TODO write a function that will accept any options from the defined list 
#TODO write another function that will accept any options outside of the defined list
setCarnivalOptions <- function(options=NULL, ...) {
  options <- c(options, ...)
  return(options)
}

suggestedCplexOptions <- function() {
  options <- list(
    mipGap = 0.05,
    poolrelGap = 0.0001,
    limitPop = 500,
    poolCap = 100,
    poolIntensity = 4,
    poolReplace = 2
  )
  return(options)
}

#TODO options list from the cplex itself
#TODO careful with scientific notation, it is switched off at another place in the code (look up for scipen)
defaultCplexOptions <- function() {
  options <- list(
        mipGap = 1e-04, 
        poolrelGap = 1e75,
        limitPop = 20,
        poolCap = 2.1e9,
        poolIntensity = 0,
        poolReplace = 0
    )
    return(options)
}

#TODO what other params are needed here
defaultLpSolveCarnivalOptions <- function() {
    
  options <- list(
        solver=supportedSolvers$lpSolve
    )
    
    return(options)
}


