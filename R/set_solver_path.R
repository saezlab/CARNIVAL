#' searchForCPLEXSolver
#' 
#' search for interactive cplex executable in the default path
#' 
#' @return path to CPLEX if found or empty string ("") if not. 
#TODO add windows support
#check https://www.ibm.com/docs/en/icos/12.8.0.0?topic=v1280-installing-cplex-optimization-studio
searchForCPLEXSolver <- function() {
  cplexBinaryLocation <- ""
  if (.Platform$OS.type %in% c("unix", "linux")) {
    #mac
    cplexStudioLocation <- list.files("/Applications", pattern = "CPLEX_Studio", 
                                      ignore.case = TRUE,
                                      full.names = TRUE)[1]
    
    if (length(cplexStudioLocation) == 0) {
     #UNIX & linux
     cplexStudioLocation <- list.files("/opt/ibm/ilog", pattern = "CPLEX_Studio", 
                                       ignore.case = TRUE,
                                       full.names = TRUE)[1]
    }
    
    cplexStudioLocation <- list.files(paste0(cplexStudioLocation, "/cplex/bin"), 
                                      full.names = TRUE)[1]
    cplexBinaryLocation <- paste0(cplexStudioLocation, "/cplex")
    
    if(testRunCplex(cplexBinaryLocation)){
        return(cplexBinaryLocation)
    }else{
        return("")
    }
  } else {
    message("Automatic setup path for cplex solver is not support for Windows yet.")
      cplexBinaryLocation = ""
  }
  
  return(cplexBinaryLocation)
}
  
#' checks if the the cplexPath exists
#' 
#' tries to interact with the cplex 
#' @param cplexPath path to cplex to check
#' @return TRUE if interaction was successful, FALSE otherwise
testRunCplex <- function(cplexPath) {
    
    isSuccessfull <- tryCatch({
            results = system(paste(cplexPath, "-c quit"), intern = TRUE)
            any(grepl("Welcome", results))
        },
        error = function(cond){
            return(FALSE)
        })
}

testRunLpSolve <- function(){
  #simple lp problem (copied from lpSolve examples)
  chess.obj <- rep (1, 64)
  q8 <- make.q8 ()
  chess.dir <- rep (c("=", "<"), c(16, 26))
  chess.rhs <- rep (1, 42)
  
  results <- lp (direction = 'max', objective.in = chess.obj,
                 const.dir = chess.dir, const.rhs =  chess.rhs, dense.const = q8, 
                all.bin=TRUE, num.bin.solns=3)
  isSuccessfull <- results$status == 0 
  
  if (isSuccessfull) {
    message("Test run finished successfully.")  
  } else {
    stop("Test run has failed. Solver lpSolve cannot be run.", 
         paste(results, collapse = "\n"))
  }
  return(isSuccessfull)
}

testRunCbc <- function(cbcPath) {
    
    if(!file.exists(cbcPath)) return(FALSE)
    
  message("Test run of cbc")
  results <- system(paste(cbcPath, "simpleIlpProblem.lp"), intern = TRUE)
  isSuccessfull <- any(grepl("Welcome", results)) && any(grep("Total time", results))
  if (isSuccessfull) {
    message("Test run finished successfully.")  
  } else {
    stop("Test run has failed. Solver cbc cannot be run.", 
         paste(results, collapse = "\n"))
  }
  return(isSuccessfull)
}

addSolverPathToEnvironement <- function(cplexSolverPath) {
  rEnvironementFile <- paste0(R.home(), "/etc/", "Renviron")
  lineToWrite <- paste0("cplexSolverPath=", cplexSolverPath)
  write(lineToWrite, rEnvironementFile, append = TRUE)
}

checkSolverPathInEnvironement <- function(solver) {
  return(Sys.getenv()['cplexSolverPath'])
}

checkSolverPathInSystem <- function(solver) {
  solverPath <- checkSolverPathInEnvironement(solver)
  message("Solver path was set by environment to:", solverPath)
  if (is.na(solverPath)) {
    solverPath <- searchForSolver(solver)
    if (solverPath != "") {
      addSolverPathToEnvironement(solverPath)
      message("Solver path was automatically set to:", solverPath)
    }
  } 
  
  return(solverPath)
}
