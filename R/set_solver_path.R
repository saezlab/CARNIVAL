#TODO add windows support
#check https://www.ibm.com/docs/en/icos/12.8.0.0?topic=v1280-installing-cplex-optimization-studio
searchForSolver <- function(solver) {
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
  } else {
    message("Automatic setup path for cplex solver is not support for Windows yet.")
  }
  
  return(cplexBinaryLocation)
}
  
testRunCplex <- function() {
  cplexPath <- checkSolverPathInEnvironement("cplex")
  lineToCplex <- "read simpleIlpProblem.lp optimize"
  message("Test run of cplex")
  results <- system(paste(cplexPath, "-c", lineToCplex), intern = TRUE) 
  isSuccessfull <- any(grepl("Welcome", results)) && any(grep("Solution time", results))
  
  if (isSuccessfull) {
    message("Test run finished successfully.")  
  } else {
    stop("Test run has failed. Solver lpSolve cannot be run.", 
         paste(results, collapse = "\n"))
  }
}

testRunLpSolve <- function(){
  #simple lp problem (copied from lpSolve examples)
  chess.obj <- rep (1, 64)
  q8 <- make.q8 ()
  chess.dir <- rep (c("=", "<"), c(16, 26))
  chess.rhs <- rep (1, 42)
  
  results <- lp ('max', chess.obj, , chess.dir, chess.rhs, dense.const = q8, 
                all.bin=TRUE, num.bin.solns=3)
  isSuccessfull <- any(grepl("Success:", results))
  
  if (isSuccessfull) {
    message("Test run finished successfully.")  
  } else {
    stop("Test run has failed. Solver lpSolve cannot be run.", 
         paste(results, collapse = "\n"))
  }
}

testRunCbc <- function() {
  cbcPath <- system("which cbc", intern=TRUE)
  message("Test run of cbc")
  results <- system(paste(cbcPath, "simpleIlpProblem.lp"), intern = TRUE)
  isSuccessfull <- any(grepl("Welcome", results)) && any(grep("Total time", results))
  if (isSuccessfull) {
    message("Test run finished successfully.")  
  } else {
    stop("Test run has failed. Solver cbc cannot be run.", 
         paste(results, collapse = "\n"))
  }
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
