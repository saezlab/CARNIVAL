#Supports mac only atm
searchForSolver <- function(solver) {
  cplexStudioLocation <- list.files("/Applications", pattern = "CPLEX_Studio", 
                                    full.names = TRUE)[1]
  cplexStudioLocation <- list.files(paste0(cplexStudioLocation, "/cplex/bin"), 
                           full.names = TRUE)[1]
  cplexBinaryLocation <- paste0(cplexStudioLocation, "/cplex")
  return(cplexBinaryLocation)
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