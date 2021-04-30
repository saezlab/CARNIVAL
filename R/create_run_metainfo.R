#TODO add this rather to api and then to docs
availableFlavours <- list(vanilla = "vanilla", fromLp = "fromLp", 
                          inverse = "inverse")

collectMetaInfo <- function(carnivalOptions) {
  runId <- createRunId()
  carnivalOptions$runId <- runId

  filenames <- createFilenames(carnivalOptions)
  carnivalOptions$filenames <- filenames

  carnivalOptions$startTime <- getTime()
  carnivalOptions$flavour <- availableFlavours$vanilla

  return(carnivalOptions)
}

createRunId <- function() {
  datetime <- format(Sys.time(), "t%H_%M_%Sd%d_%m_%Y")
  salt <- sample(1:100, 1)
  runId <- paste(datetime, salt, sep="n")

  return(runId)
}

createFilenames <- function(carnivalOptions) {
  #TODO add windows support with .Platform
  if( carnivalOptions$outputFolder != "" ) {
    outputFolder <- file.path(carnivalOptions$outputFolder, "")  
  } else {
    outputFolder <- file.path(getwd(), "")
  }
  
  lpFilename <- paste0(outputFolder, "lpFile", "_", carnivalOptions$runId, ".lp")
  parsedData <- paste0(outputFolder, "parsedData_", carnivalOptions$runId, ".RData")
  resultFile <- paste0(outputFolder, "result", "_", carnivalOptions$runId, ".txt")

  filenames <- list("lpFilename" = lpFilename, "parsedData" = parsedData,
                    "resultFile" = resultFile)

  if(carnivalOptions$solver == getSupportedSolvers()$cplex) {
    filenames <- createSolverSpecificFiles(carnivalOptions, filenames)
  }

  return(filenames)
}

createSolverSpecificFiles <- function(carnivalOptions, filenames) {
  outputFolder <- carnivalOptions$outputFolder
  if( carnivalOptions$solver == getSupportedSolvers()$cplex ) {
    cplexCommandFile <- paste0(outputFolder, "cplexCommand", "_", carnivalOptions$runId, ".txt")
    cplexLog <- paste0(outputFolder, "cplexLog", "_", carnivalOptions$runId, ".txt")
    filenames <- c(filenames, "cplexCommandFile" = cplexCommandFile,
                              "cplexLog" = cplexLog)
  }

  return(filenames)
}

getTime <- function() {
  time <- format(Sys.time(), "%H:%M:%S %d.%m.%Y")
  return(time)
}

