#'\code{checkSolver}
#'
#'Run CARNIVAL pipeline using to the user-provided list of inputs or run 
#'CARNIVAL built-in examples
#'Note: The pipeline requires either all required user-defined input variables 
#'(netObj and measObj) are set to NULL or CARNIVAL_example is set to NULL to 
#'execute
#'
#'@param solverPath Path to executable cplex file - always required
#'@param solver Solver name
#'@param dir_name Name of directory where to store DOT result
#'
#'@return Error message in case of errors in the inputs
#'
#'@export
#'
#'Enio Gjerga, 2020

checkSolver <- function(solverPath = solverPath, solver = solver,
                        dir_name = dir_name){
  
  if(!is.null(solverPath)){
    if(!is(solverPath, "character")){
      stop("SolverPath should be of type character")
    } else {
      if(!file.exists(solverPath)){
        stop("Please provide a valid path to interactive solver")
      }
    }
  }
  
  if(!is(solver, "character")){
    stop("solver should be of type character")
  } else {
    ## checking for solver validity (cplex/cbc/lpSolve)
    valid_solver_list <- c("cplex", "cbc", "lpSolve")
    if (!(solver %in% valid_solver_list)){
      stop(paste0("Select a valid solver option (", 
                  paste(valid_solver_list, collapse=", "), ")"))
    }
  }
  
  if(!is.null(dir_name)){
    if(!is(dir_name, "character")){
      stop("dir_name should either be NULL or provided as a character")
    }
  }
  
}