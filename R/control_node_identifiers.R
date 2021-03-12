## Warning message in case of unwanted symbols in node id's
##
## Enio Gjerga, 2020

collectSpecialCharactersNames <- function(namesWithSpecialCharacters, nodesNames) {
  counter <- 0
  res <- lapply(namesWithSpecialCharacters, function(x) {
    counter <<- counter + 1
    if (x[1] != -1){
      substr(names[counter], x[1], x[1])  
    }
  })
  names(res) <- nodesNames
  
  return(res)
}

#TODO implement keeping names mapping
controlNodeIdentifiers <- function( network = network,
                                    replacementSymbol = "_",
                                    verbose=FALSE,
                                    keepMapping=FALSE ){
  specialRegExpCharacters <- c("*", "+", "=")
  substitutionCharacters <- c("-", "<", ">", "/", " ")
  
  preparedPattern <- paste0("\\", specialRegExpCharacters, collapse="|")
  preparedPattern <- paste0(c(preparedPattern, substitutionCharacters), collapse="|")
  
  if (verbose) {

    namesWithSpecialCharactersSource <- gregexpr(pattern = preparedPattern, text = network$source) 
    namesWithSpecialCharactersTarget <- gregexpr(pattern = preparedPattern, text = network$target) 
    
    resSource <- collectSpecialCharactersNames(namesWithSpecialCharactersSource, network$source)
    resTarget <- collectSpecialCharactersNames(namesWithSpecialCharactersTarget, network$target)
    
    allSpecialCharactersFound <- unique(unlist(resSource), unlist(resTarget))
    
    allSubstitutedNamesSource <- names(resSource[lengths(resSource) != 0])
    allSubstitutedNamesTarget <- names(resTarget[lengths(resTarget) != 0])
    
    warning("Your network contains node identifiers with characters ", 
            paste0(allSpecialCharactersFound, sep=", "), " and they will
            be replaced with '_'")
  }  

  network$source <- gsub(pattern = preparedPattern, x = network$source, replacement = replacementSymbol)  
  network$target <- gsub(pattern = preparedPattern, x = network$target, replacement = replacementSymbol)  
  
  return(network) 
}


