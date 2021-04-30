## Solving CARNIVAL problem. 
## 
## Enio Gjerga, Olga Ivanova 2020-2021

#' Supported solver functions to run all solvers in an uniform way.
getSupportedSolversFunctions <- function() {
  supportedSolversFunctions <- list("cplex" = c("solve" = solveWithCplex, 
                                                "getSolutionMatrix" = getSolutionMatrixCplex,
                                                "export" = exportIlpSolutionFromSolutionMatrix, 
                                                "saveDiagnostics" = saveDiagnosticsCplex),
                                    
                                    "cbc" =   c("solve" = solveWithCbc, 
                                                "getSolutionMatrix" = getSolutionMatrixCbc, 
                                                "export" = exportIlpSolutionFromSolutionMatrix), 
                                    
                                    "lpSolve" = c("solve" = solveWithLpSolve, 
                                                  "getSolutionMatrix" = getSolutionMatrixLpSolve,
                                                  "export" = exportIlpSolutionFromSolutionMatrix)) 
  return(supportedSolversFunctions)
}

prepareForCarnivalRun <- function(dataPreprocessed,
                                  carnivalOptions, 
                                  newDataRepresentation = T) {
  intDataRep <- createInternalDataRepresentation( dataPreprocessed, newDataRepresentation )
  
  if(newDataRepresentation) {
    writeParsedData( intDataRep, dataPreprocessed, carnivalOptions )
    lpFormulation <- createLpFormulation_v2( intDataRep, dataPreprocessed, 
                                             carnivalOptions )
    variables <- intDataRep 
    
  } else {
    writeParsedData( intDataRep[[2]], dataPreprocessed, carnivalOptions )
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
  
  return(variables)
}


solveCarnivalFromLp <- function(lpFile = "", 
                                parsedDataFile = "",
                                newDataRepresentation = T,
                                carnivalOptions) {
  load(parsedDataFile) 

  carnivalOptions$filenames$lpFilename <- lpFile
  solutionMatrix <- sendTaskToSolver( variables, dataPreprocessed, carnivalOptions )
  result <- processSolution( solutionMatrix, variables, newDataRepresentation, 
                             dataPreprocessed, carnivalOptions )
  
  return(result)
}

solveCarnival <- function( dataPreprocessed,
                           carnivalOptions, 
                           newDataRepresentation = T ) {
  
  variables <- prepareForCarnivalRun(dataPreprocessed, carnivalOptions, newDataRepresentation)
  solutionMatrix <- sendTaskToSolver(variables, dataPreprocessed, carnivalOptions)
  
  print(solutionMatrix)
  
  if (ncol(solutionMatrix) == 0) {
    message("No solutions exist.")
  } else {
    result <- processSolution( solutionMatrix, variables, newDataRepresentation, 
                               dataPreprocessed, carnivalOptions )  
  }
  
  #TODO results with diagnostics is never null, think how to implement it better
  #if (!is.null(result)) {
  #  writeDotFigure(result = result,
  #              dir_name = outputFolder,
  #              inputs = perturbations,
  #              measurements = measurements,
  #              UP2GS = FALSE)
  #}
  
  return(result)
}

sendTaskToSolver <- function( variables,
                              dataPreprocessed, 
                              carnivalOptions,
                              newDataRepresentation = T) {
  
  message(getTime(), " Solving LP problem")
  
  supportedSolversFunctions <- getSupportedSolversFunctions()
  solversFunctions <- supportedSolversFunctions[[carnivalOptions$solver]]
  
  #lpSolve uses matrix input for variables, other solvers take .lp file
  if(carnivalOptions$solver == getSupportedSolvers()$lpSolve) {
    if(newDataRepresentation) {
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


processSolution <- function(solutionMatrix, 
                            variables,
                            newDataRepresentation = T,
                            dataPreprocessed,
                            carnivalOptions) {
  
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


createInternalDataRepresentation <- function( dataPreprocessed, newDataRepresentation = T ) {
  if (newDataRepresentation) {
    variables <- createVariablesForIlpProblem(dataPreprocessed)
    return(variables)
    
  } else {
    dataVector <- buildDataVector(dataPreprocessed$measurements, 
                                  dataPreprocessed$priorKnowledgeNetwork, 
                                  dataPreprocessed$perturbations)
    
    variables <- createVariables(dataPreprocessed$priorKnowledgeNetwork, dataVector)
    return(list("dataVector" = dataVector, "variables" = variables))
  }
}


writeParsedData <- function ( variables = variables, 
                              dataPreprocessed = dataPreprocessed, 
                              carnivalOptions = carnivalOptions,
                              filename = "parsedData.RData") {
  message("Saving parsed data")
  
  outputFolder <- carnivalOptions$outputFolder
  parsedDataFilename <-carnivalOptions$filenames$parsedData
  save(variables, 
       dataPreprocessed,
       file = parsedDataFilename)
  
  message("Done: saving parsed data: ", parsedDataFilename)
}