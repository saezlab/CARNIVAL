## Checking the measurements provided, controlling the identifiers for special characters
## Collecting and providing the IDs for measurements nodes that cannot be found in prior knowledge
## network
## Enio Gjerga, Olga Ivanova 2020-2021

checkMeasurements <- function(measurements, nodesPriorKnowledgeNetwork) {
  
  nullObjectError <- "Please provide a valid measurement object: a vector with identifiers and their values."
  noMeasurementsInNetworkError <- "None of your measurements are in prior knowledge networks (PKN). 
                                   Check node identifiers in both measurements and PKN objects."
  
  if (is.null(measurements) || !is.numeric(measurements)) {
    stop(nullObjectError)
  }
  
  names(measurements) <- correctIdentifiers(names(measurements))

  measurementsNotInNetwork <- measurements[!names(measurements) %in% nodesPriorKnowledgeNetwork]
  measurementsProcessed <- measurements[names(measurements) %in% nodesPriorKnowledgeNetwork] 
  
  if (length(measurementsNotInNetwork) == length(measurementsProcessed)) {
    stop(noMeasurementsInNetworkError)
  }
  
  if ( length(measurementsNotInNetwork) > 0 ) {
    warning("These nodes are not in prior knowledge network and will be ignored: ", 
            names(measurementsNotInNetwork))   
  }
  
  return(measurementsProcessed)
}
