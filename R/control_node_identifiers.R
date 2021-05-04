## Providing functions for fixing special characters in node identifiers
##
## Enio Gjerga, Olga Ivanova 2020-2021

specialRegExpCharactersToFix <- c("*", "+", "=")
specialCharactersToFix <- c("-", "<", ">", "/", " ")

collectSpecialCharactersNames <- function(namesWithSpecialCharacters, nodesNames) {
  counter <- 0
  res <- lapply(namesWithSpecialCharacters, function(x) {
    counter <<- counter + 1
    if (x[1] != -1) {
      substr(names[counter], x[1], x[1])  
    }
  })
  names(res) <- nodesNames
  
  return(res)
}

correctIdentifiers <- function(nodesIds, replacementSymbol = "_",
                               verbose = FALSE, keepMapping = FALSE ){
  
  preparedPattern <- paste0("\\", specialRegExpCharactersToFix, collapse = "|")
  preparedPattern <- paste0(c(preparedPattern, specialCharactersToFix), collapse = "|")
  
  if (verbose) {
    idsWithSpecialCharacters <- gregexpr(pattern = preparedPattern, text = nodesIds) 
    
    resultsIds <- collectSpecialCharactersNames(idsWithSpecialCharacters, nodesIds)
    
    allSpecialCharactersFound <- unique(unlist(nodesIds))
    allSubstitutedNamesTarget <- names(resultsIds[lengths(resultsIds) != 0])
    
    warning("Provided nodes have identifiers with characters ", 
            paste0(allSpecialCharactersFound, sep=", "), " and they will
            be replaced with '_'")
  }  
  
  nodesIds <- gsub(pattern = preparedPattern, x = nodesIds, replacement = replacementSymbol)  
  return(nodesIds) 
  
}

correctNodeIdentifiersInNetwork <- function( network, replacementSymbol = "_", verbose = FALSE,
                                             keepMapping = FALSE ){
  
  network$source <- correctIdentifiers(network$source, replacementSymbol, verbose, keepMapping)
  network$target <- correctIdentifiers(network$target, replacementSymbol, verbose, keepMapping)
  
  return(network) 
}


