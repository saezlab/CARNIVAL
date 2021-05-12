# Author: Olga Ivanova, 2021

#' Supported solvers functions to work with all solvers in a uniform way.
#' 
#' To add a new solver, one must write and add here the functions for 
#' 3 steps: solve, obtaining a solution matrix, exporting the solution
#' matrix. More specific functions can be written and called (e.g. check 
#' saveDiagnostics in cplex).
#' 
#' @return list of solvers and their corresponding functions.
#' @keywords internal
getSupportedSolversFunctions <- function() {
  solversFunctions <- list("cplex" =  c("solve" = solveWithCplex, 
                                        "getSolutionMatrix" = getSolutionMatrixCplex,
                                        "export" = exportIlpSolutionFromSolutionMatrix, 
                                        "saveDiagnostics" = saveDiagnosticsCplex),
                                    
                          "cbc" =     c("solve" = solveWithCbc, 
                                        "getSolutionMatrix" = getSolutionMatrixCbc, 
                                        "export" = exportIlpSolutionFromSolutionMatrix), 
                                    
                          "lpSolve" = c("solve" = solveWithLpSolve, 
                                        "getSolutionMatrix" = getSolutionMatrixLpSolve,
                                        "export" = exportIlpSolutionFromSolutionMatrix)) 
  return(solversFunctions)
}


#' Prepares ILP formulation and writes it to .lp file. 
#' Currently supports the old data representation (CARNIVAL v.<2) 
#' for debugging and testing if any problems arise with the new way 
#' to generate variables. 
#'
#' @inheritParams solveCarnival
#'
#' @return list with all variables and ILP formulation written in .lp file.
#' @keywords internal
prepareForCarnivalRun <- function(dataPreprocessed, carnivalOptions, 
                                  newDataRepresentation = T) {
  
  intDataRep <- createInternalDataRepresentation(dataPreprocessed, 
                                                 newDataRepresentation)
  parsedDataFilename <- carnivalOptions$filenames$parsedData
  
  if (newDataRepresentation) {
    writeParsedData( intDataRep, dataPreprocessed, parsedDataFilename )
    lpFormulation <- createLpFormulation_v2( intDataRep, dataPreprocessed, 
                                             carnivalOptions )
    variables <- intDataRep 
    
  } else {
    #previous data representation had two data structures, variables were in 2nd
    writeParsedData( intDataRep[[2]], dataPreprocessed, parsedDataFilename )
    lpFormulation <- createLpFormulation( intDataRep, dataPreprocessed, 
                                          carnivalOptions )
    variables <- intDataRep[[2]]
  }
  
  writeSolverFile(objectiveFunction = lpFormulation$objectiveFunction,
                  allConstraints = lpFormulation$allConstraints,
                  bounds = lpFormulation$bounds,
                  binaries = lpFormulation$binaries,
                  generals = lpFormulation$generals,
                  carnivalOptions = carnivalOptions)
  
  preparedForRun <- list("variables" = variables, 
                         "lpFormulation" = lpFormulation)
  return(preparedForRun)
}


#' Sends the ILP formulation defined in .lp file to solver. Uses parsedDataFile
#' to process the final solution and map the ILP variables back to inital 
#' data. 
#'
#' @param lpFile path to .lp file that will be used to run the solver.
#' @param parsedDataFile path to parsed data file that was created after running 
#' \code{\link{prepareForCarnivalRun}} or in previous CARNIVAL runs.
#' @inheritParams solveCarnival
#'
#' @return solution of ILP problem
#' @keywords internal
solveCarnivalFromLp <- function(lpFile = "", 
                                parsedDataFile = "",
                                carnivalOptions,
                                newDataRepresentation = T) {

  load(parsedDataFile, loadedData <- new.env()) 

  carnivalOptions$filenames$lpFilename <- lpFile
  solutionMatrix <- sendTaskToSolver( loadedData$variables, 
                                      loadedData$dataPreprocessed, 
                                      carnivalOptions )
  
  if (ncol(solutionMatrix) == 0) {
    message("No solutions exist.")
  } else {
    solution <- processSolution( solutionMatrix, loadedData$variables, 
                                 loadedData$dataPreprocessed, 
                                 carnivalOptions,
                                 newDataRepresentation )  
  }
  
  return(solution)
}

#' Main CARNIVAL function to execute the full pipeline: 
#' 1) preprocess the data
#' 2) prepare ILP formulation
#' 3) executes the solver on ILP formulation 
#' 4) parse the output of the solver and map it to the original data.
#'
#' @param dataPreprocessed list containing preprocessed priorKnowledgeNetwork, 
#' measurements, weights (if provided), perturbations (if provided).
#' @param carnivalOptions all options of CARNIVAL.
#' @param newDataRepresentation TRUE by default. For debugging with the old 
#' data representation, put to FALSE.
#'
#' @return solution of the ILP problem.
#' @keywords internal
solveCarnival <- function(dataPreprocessed,
                          carnivalOptions, 
                          newDataRepresentation = T) {
  
  preparedForRun <- prepareForCarnivalRun(dataPreprocessed, carnivalOptions, 
                                          newDataRepresentation)
  solutionMatrix <- sendTaskToSolver(preparedForRun$variables, dataPreprocessed, 
                                     carnivalOptions)
  
  if (ncol(solutionMatrix) == 0) {
    message("No solutions exist.")
  } else {
    solution <- processSolution( solutionMatrix, preparedForRun$variables, 
                                 dataPreprocessed, carnivalOptions, 
                                 newDataRepresentation )  
  }
  
  return(solution)
}


#' Executes the solve on the provided ILP formulation (in .lp file). 
#'
#' @param variables list of nodes, edges and measurements variables 
#' generated by createLpFormulation_v2.
#' @inheritParams solveCarnival
#'
#' @return solution matrix from ILP solver containing variables list (rows) and 
#' their values in different solutions (columns). 
#' @keywords internal
sendTaskToSolver <- function(variables,
                            dataPreprocessed, 
                            carnivalOptions,
                            newDataRepresentation = T) {
  
  message(getTime(), " Solving LP problem")
  
  supportedSolversFunctions <- getSupportedSolversFunctions()
  solversFunctions <- supportedSolversFunctions[[carnivalOptions$solver]]
  
  #lpSolve uses matrix input for variables, other solvers take .lp file
  if (carnivalOptions$solver == getSupportedSolvers()$lpSolve) {
    if (newDataRepresentation) {
      lpMatrix <- transformVariables_v2(variables, dataPreprocessed$measurements)
    } else {
      lpMatrix <- transformVariables(variables, dataPreprocessed$measurements)
    }
    lpSolution <- solversFunctions$solve(lpMatrix, carnivalOptions)
  } else {
    lpSolution <- solversFunctions$solve(carnivalOptions)  
  }

  message(getTime(), " Done: solving LP problem.")
  
  message(getTime()," Getting the solution matrix")
  solutionMatrix <- solversFunctions$getSolutionMatrix(lpSolution)
  message(getTime(), " Done: getting the solution matrix.")
  
  return(solutionMatrix)
}


#' Exports the solution matrix to the final solution.
#'
#' @param solutionMatrix the output matrix from ILP solver containing 
#' variables list (rows) and their values in different solutions (columns).
#' @inheritParams sendTaskToSolver
#'
#' @keywords internal
processSolution <- function(solutionMatrix, 
                            variables,
                            dataPreprocessed,
                            carnivalOptions, 
                            newDataRepresentation = T) {
  
  message(getTime(), " Exporting solution matrix")
  
  supportedSolversFunctions <- getSupportedSolversFunctions()
  solversFunctions <- supportedSolversFunctions[[carnivalOptions$solver]]
  
  if (newDataRepresentation) {
    result <- solversFunctions$export( solutionMatrix, variables )
  } else {
    result <- exportIlpSolutionResultFromXml( solMatrix = solutionMatrix, 
                                              variables = variables, 
                                              dataPreprocessed )
  }
  
  if (carnivalOptions$solver == getSupportedSolvers()$cplex) {
    result <- solversFunctions$saveDiagnostics(result, carnivalOptions)
  } 
  
  message(getTime(), " Done: exporting solution matrix.")
  return(result)
}


#' Creates internal data representation - variables for ILP solvers, on the 
#' basis of provided preprocessed data.
#'
#' @inheritParams solveCarnival
#'
#' @return variables for the new data representation or
#' data vector (containing preprocessed information on measurement) 
#' and variables for the old data representation (CARNIVAL v.<2)
#' 
#' @keywords internal
createInternalDataRepresentation <- function(dataPreprocessed, 
                                            newDataRepresentation = T) {
  if (newDataRepresentation) {
    variables <- createVariablesForIlpProblem(dataPreprocessed)
    return(variables)
    
  } else {
    dataVector <- buildDataVector(dataPreprocessed$measurements, 
                                  dataPreprocessed$priorKnowledgeNetwork, 
                                  dataPreprocessed$perturbations)
    
    variables <- createVariables(dataPreprocessed$priorKnowledgeNetwork, 
                                 dataVector)
    return(list("dataVector" = dataVector, "variables" = variables))
  }
}


#' Saves all provided data together with generated variables for ILP problem in 
#' .RData file.
#'
#' @param dataPreprocessed list containing preprocessed priorKnowledgeNetwork, 
#' measurements, weights (if provided), perturbations (if provided).
#' @param variables list of nodes, edges and measurements variables 
#' generated by createLpFormulation_v2
#' @param filename filename of the parsed data file.
#'
#' @return filename of the parsed data file.
#' @keywords internal
writeParsedData <- function(variables = variables, 
                            dataPreprocessed = dataPreprocessed, 
                            filename = "parsedData.RData") {
  message("Saving preprocessed data.")
  save(variables, dataPreprocessed, file = filename)
  message("Done: saving parsed data: ", filename)
  return(filename)
}
