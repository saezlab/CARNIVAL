#' default_CARNIVAL_options
#' 
#' generates default CARNIVAL options.  
#' 
#' @return returns a list with all possible options implemented in CARNIVAL.
#' see the documentation on \code{\link{CARNIVAL::runCARNIVAL}}.
#' @export
#' 


supportedSolvers <- list(cplex="cplex", cbc="cbc", lpSolve="lpSolve")
requiredCarnivalCplexOptions <- c("solverPath", "solver", "timelimit", "alphaWeight", "betaWeight", "threads", "dirName")
requiredCplexOptions <- c("mipGap", "poolrelGap", "limitPop", "poolCap", "poolIntensity", "poolReplace")

defaultCplexCarnivalOptions <- function(solverPath=""){
    
    options <- list(
         solverPath=solverPath,
         solver=supportedSolvers$cplex, 
         timelimit=3600, 
         mipGap=0.05,
         alphaWeight=1, 
         betaWeight=0.2,
         threads=1,
         cplexMemoryLimit=8192,
         cleanTmpFiles=TRUE,
         keepLPFiles=TRUE,
         dirName=NULL
    )
    
    options <- c(options, defaultCplexOptions())
    return(options)
}

#TODO write a function that will accept any options from the defined list 
#TODO write another function that will accept any options outside of the defined list
setCarnivalOptions <- function(options=NULL, ...) {
  options <- c(options, ...)
  
  return(options)
}

#TODO options list from the cplex itself
defaultCplexOptions <- function() {
  options <- list(
        mipGap=1e-04, 
        poolrelGap=1e75,
        limitPop=20,
        poolCap=2.1e9,
        poolIntensity=0,
        poolReplace=0
    )
    return(options)
}

#TODO ask Enio what those indices represent and how they were supposed to be used
setParallelRuns <- function(idx1, idx2) {
  parallelIdx1=1
  parallelIdx2=1
  if(!is.numeric(parallelIdx1) | !is.numeric(parallelIdx2)){
    stop("Please set numbers on the parameters 'parallelIdx1' and 'parallelIdx2'
         for running CARNIVAL in parallelisation ")
  } else {
    if(parallelIdx1 == 1 & parallelIdx2 == 1) { # default case
      repIndex=1;condition=1
    } else {
      condition=parallelIdx1;repIndex=parallelIdx2
    }
  }
  return(c("condition"=condition, "repIndex"=repIndex))
}

#TODO what other params are needed here
defaultLpSolveCarnivalOptions <- function() {
    
  options <- list(
        solver=supportedSolvers$lpSolve
    )
    
    return(options)
}


