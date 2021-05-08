processOneInequality <- function(inequality, allVariables) {
  #removing constraint number from the string
  inequality[[1]] <- gsub("c[0-9]*:\t", "", inequality[[1]])
  
  inequalityPartsSeparators <- c( "<=", ">=", "=", "<", ">")
  operators <- c("-", "+")
  
  partsSeparator <- which(inequality %in% inequalityPartsSeparators)
  leftPart <- inequality[1:(partsSeparator - 1)]
  rightPart <- inequality[(partsSeparator + 1):length(inequality)]
  sepOperator <- inequality[partsSeparator]
  
  leftPart[leftPart == "+"] <- 1
  leftPart[leftPart == "-"] <- -1
  
  #if the line starts with variable, add coefficient 1 to it
  if (leftPart[1] %in% allVariables) {
    leftPart <- c(1, leftPart)
  } 
  
  coefficients <- leftPart[seq(1, length(leftPart), 2)]
  coefficients <- as.numeric(coefficients)
  vars <- leftPart[seq(2, length(leftPart), 2)]
  names(coefficients) <- vars
  
  varsValues <- rep(0, length(allVariables))
  names(varsValues) <- allVariables
  varsValues[names(coefficients)] <- coefficients
  
  return(list("left" = varsValues, "operator" = sepOperator, "right" = rightPart))
}

transformInequalititesToMatrix <- function(inequalities, allVariables) {
  inequalities <- trimws(inequalities)

  inequalitiesSplit <- strsplit(inequalities, " ")
  allTransformedInequalities <- sapply(inequalitiesSplit, processOneInequality, 
                                      allVariables)
  leftParts <- do.call(rbind, allTransformedInequalities[1, ])
  operator <- unlist(allTransformedInequalities[2, ])
  rightParts <- unlist(allTransformedInequalities[3, ])
  
  return(list("left" = leftParts, "operator" = operator, "right" = rightParts))
}
