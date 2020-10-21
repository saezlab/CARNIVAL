#' default_CARNIVAL_options
#' 
#' generates default CARNIVAL options.  
#' 
#' @return returns a list with all possible options implemented in CARNIVAL.
#' see the documentation on \code{\link{CARNIVAL::runCARNIVAL}}.
#' @export
#' 

#TODO add Attila as authors

supportedSolvers <- list(cplex="cplex", cbc="cbc", lpSolve="lpSolve")

defaultCplexCarnivalOptions <- function(){
    
    opts <- list(
         solverPath=NULL,
         solver=supportedSolvers$cplex, 
         timelimit=3600, 
         mipGap=0.05,
         poolrelGap=0.0001,
         limitPop=500, 
         poolCap=100,
         poolIntensity=4,
         poolReplace=2,
         alphaWeight=1, 
         betaWeight=0.2,
         threads=1,
         cplexMemoryLimit=8192,
         dirName=NULL
    )
    return(opts)
}

#TODO options list from the cplex itself
defaultCplexOptions <- function() {
    opts <- list(
        mipGap=1e-04, 
        poolrelGap=1e75,
        limitPop=20,
        poolCap=2.1e9,
        poolIntensity=0,
        poolReprace=0
    )
    return(opts)
}

#TODO what other params are needed here
defaultLpSolveCarnivalOptions = function() {
    
    opts <- list(
        solver=supportedSolvers$lpSolve
    )
    
    return(opts)
}


#' check_CARNIVAL_options
#' 
#' checks options provided for CARNIVAL
#' 
checkCplexCarnivalOptions <- function(opts){
    
    if(!is.list(opts)) stop("CARNIVAL options should be a list")
    requiredNames <- c(
        "solverPath",
        "solver", 
        "timelimit",
        "mipGap",
        "poolrelGap",
        "limitPop", 
        "poolCap",
        "poolIntensity",
        "poolReplace",
        "alphaWeight", 
        "betaWeight",
        "threads",
        "dirName")
    
    if(!all(requiredNames %in% names(opts))){
        stop("CARNIVAL options should contain all options. 
             Start by calling default_carnival_options() and replace entries. ")
    }
    
    
    if(is.null(opts$solverPath)) stop("path to ILP solver must be provided")
    
}
