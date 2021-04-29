## Solving CARNIVAL problem. 
## 
## Enio Gjerga, Olga Ivanova 2020-2021

#' Supported solver functions to run all solvers in an uniform way.
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
    
    if(carnivalOptions$solver == supportedSolvers$lpSolve) {
      variables <- transformVariables(variables, dataPreprocessed$measurements)
    }
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
                           newDataRepresentation = T) {
  
  variables <- prepareForCarnivalRun(dataPreprocessed, carnivalOptions, newDataRepresentation)
  solutionMatrix <- sendTaskToSolver( variables, dataPreprocessed, carnivalOptions )
  result <- processSolution( solutionMatrix, variables, newDataRepresentation, 
                             dataPreprocessed, carnivalOptions )
  
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
                              carnivalOptions ) {
  
  message(getTime(), " Solving LP problem")
  
  solversFunctions <- supportedSolversFunctions[[carnivalOptions$solver]]
  
  #TODO remove variables, we don't need them for everything except lpSolver
  lpSolution <- solversFunctions$solve( variables = variables, 
                                        carnivalOptions = carnivalOptions,
                                        dataPreprocessed )
  message(getTime(), " Done: solving LP problem.")
  
  message(getTime()," Getting the solution matrix")
  solutionMatrix <- solversFunctions$getSolutionMatrix( lpSolution )
  message(getTime(), " Done: getting the solution matrix.")
  
  return(solutionMatrix)
}


processSolution <- function(solutionMatrix, 
                            variables,
                            newDataRepresentation = T,
                            dataPreprocessed,
                            carnivalOptions) {
  
  message(getTime(), " Exporting solution matrix")
  solversFunctions <- supportedSolversFunctions[[carnivalOptions$solver]]
  
  if (newDataRepresentation) {
    result <- solversFunctions$export( solutionMatrix = solutionMatrix, 
                                       variables = variables )
  } else {
    result <- exportIlpSolutionResultFromXml( solMatrix = solutionMatrix, 
                                              variables = variables, 
                                              dataPreprocessed )
  }
  
  if (carnivalOptions$solver == supportedSolvers$cplex) {
    result <- solversFunctions$saveDiagnostics(result, carnivalOptions)
  } 
  
  message(getTime(), " Done: exporting solution matrix.")
  return(result)
}


createInternalDataRepresentation <- function( dataPreprocessed, newDataRepresentation = F ) {
  if (newDataRepresentation) {
    variables <- createVariablesForIlpProblem(dataPreprocessed)
    return(variables)
    
  } else {
    dataVector <- buildDataVector(measurements = dataPreprocessed$measurements, 
                                  priorKnowledgeNetwork = dataPreprocessed$priorKnowledgeNetwork, 
                                  perturbations = dataPreprocessed$perturbations)
    
    variables <- createVariables(priorKnowledgeNetwork = dataPreprocessed$priorKnowledgeNetwork, 
                                 dataVector = dataVector)
    return(list("dataVector" = dataVector, "variables" = variables))
  }
}


writeParsedData <- function ( variables = variables, 
                              dataPreprocessed = dataPreprocessed, 
                              carnivalOptions = carnivalOptions,
                              filename="parsedData.RData") {
  message("Saving parsed data")
  
  outputFolder <- carnivalOptions$outputFolder
  parsedDataFilename <-carnivalOptions$filenames$parsedData
  save(variables, 
       dataPreprocessed,
       file = parsedDataFilename)
  message("Done: saving parsed data: ", parsedDataFilename)
}