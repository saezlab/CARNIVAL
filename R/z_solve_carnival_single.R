## Solving CARNIVAL problem 
##
## Enio Gjerga, 2020

supportedSolversFunctions <- list("cplex" = c("solve" = solveWithCplex, 
                                              "getSolutionMatrix" = getSolutionMatrixCplex,
                                              "export" = exportIlpSolutionResultFromXml, 
                                              "saveDiagnostics" = saveDiagnosticsCplex),
                                  
                                  "cbc" =   c("solve" = solveWithCbc, 
                                              "getSolutionMatrix" = getSolutionMatrixCbc, 
                                              "export" = exportIlpSolutionResultFromXml), 
                                  
                                  "lpSolve" = c("solve" = solveWithLpSolve, 
                                                "getSolutionMatrix" = getSolutionMatrixLpSolve,
                                                "export" = exportIlpSolutionResultFromXml))

solveCarnivalSingleFromLp <- function(#lpFile = "", 
                                      parsedDataFile = "",
                                      carnivalOptions) {
  load(parsedDataFile) 
  dataPreprocessed$measurements <- measurements
  dataPreprocessed$priorKnowledgeNetwork <- priorKnowledgeNetwork
  dataPreprocessed$perturbations <- perturbations
  
  result <- solveCarnivalSingleRun( dataPreprocessed,
                                    carnivalOptions )
  
  return(result)
}

solveCarnivalSingleRun <- function( dataPreprocessed,
                                    carnivalOptions ) {
  
  intDataRep <- createInternalDataRepresentation( measurements = dataPreprocessed$measurements, 
                                                  priorKnowledgeNetwork = dataPreprocessed$priorKnowledgeNetwork, 
                                                  perturbations = dataPreprocessed$perturbations )
  writeParsedData( variables = intDataRep[[2]], 
                   dataPreprocessed, 
                   carnivalOptions )
  
  lpFormulation <- createLpFormulation( intDataRep, dataPreprocessed, carnivalOptions )

  writeSolverFile(objectiveFunction = lpFormulation$objectiveFunction,
                  allConstraints = lpFormulation$allConstraints,
                  bounds = lpFormulation$bounds,
                  binaries = lpFormulation$binaries,
                  generals = lpFormulation$generals,
                  carnivalOptions = carnivalOptions)
  
  result <- sendTaskToSolver( variables = intDataRep[[2]],
                              dataPreprocessed, 
                              carnivalOptions )
  
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
  
  message("Solving LP problem...")
  
  solversFunctions <- supportedSolversFunctions[[carnivalOptions$solver]]
  
  lpSolution <- solversFunctions$solve( variables = variables, 
                                        carnivalOptions = carnivalOptions,
                                        dataPreprocessed )
  
  solutionMatrix <- solversFunctions$getSolutionMatrix( lpSolution )
  result <- solversFunctions$export( solMatrix = solutionMatrix, 
                                     variables = variables, 
                                     dataPreprocessed )
  
  if (carnivalOptions$solver == supportedSolvers$cplex) {
    result <- solversFunctions$saveDiagnostics(result, carnivalOptions)
  } 
  
  return(result)
}

createInternalDataRepresentation <- function( measurements = measurements, 
                                              priorKnowledgeNetwork = priorKnowledgeNetwork, 
                                              perturbations = perturbations ) {
  
  dataVector<- buildDataVector(measurements = measurements, 
                               priorKnowledgeNetwork = priorKnowledgeNetwork, 
                               perturbations = perturbations)
  
  variables <- createVariables(priorKnowledgeNetwork = priorKnowledgeNetwork, 
                               dataVector = dataVector)
  
  return(list("dataVector" = dataVector, "variables" = variables))
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
}



