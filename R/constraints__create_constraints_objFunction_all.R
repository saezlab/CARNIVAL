## This function returns the list of constraints associated with the 'Absolute 
## Difference' variables and which measure the mis-fit between inferred and 
## measured data.
## 
## Enio Gjerga, 2020


## ============================================ ##
## === Load write_constraints_objFunction.R === ##
## ============================================ ##

createConstraintsObjectiveFunction <- function(variables = variables, 
                                               dataVector = dataVector){
  
  measurements <- dataVector$dataVectorSign
  
  idx2 <- which(measurements == 1)
  idx3 <- which(measurements == -1)
  
  cc1 <- rep("", length(measurements))
  cc2 <- rep("", length(measurements))
  
  cc1[idx2] <- paste0(variables$variables[idx2], " - absDiff", 
                      idx2, " <= 1")
  cc2[idx2] <- paste0(variables$variables[idx2], " + absDiff", idx2, " >= 1")
  
  cc1[idx3] <- paste0(variables$variables[idx3], " - absDiff", idx3, " <= -1")
  cc2[idx3] <- paste0(variables$variables[idx3], " + absDiff", idx3, " >= -1")
  
  constraints0 <- c(cc1, cc2)
  
  return(constraints0[-which(constraints0=="")])
  
} 

createConstraintsMeasuredNodes_newIntRep(variables = variables) {
  positiveMeasurements <- variables$measurementsDf[variables$measurementsDf$value > 0, ]
  negativeMeasurements <- variables$measurementsDf[variables$measurementsDf$value < 0, ]
  
  if( dim(positiveMeasurements)[1] > 0 ) {
    cOf1 <<- createConstraint(positiveMeasurements$nodesVars, "-", 
                              positiveMeasurements$measurementsVars,
                              "<=", 1)
    cOf2 <<- createConstraint(positiveMeasurements$nodesVars, "+", 
                              positiveMeasurements$measurementsVars,
                               ">=", 1)
   
  }
  
  if( dim(negativeMeasurements)[1] > 0 ) {
    cOf1 <<- createConstraint(negativeMeasurements$nodesVars, "-", 
                              negativeMeasurements$measurementsVars,
                             "<=", -1)
    cOf2 <<- createConstraint(negativeMeasurements$nodesVars, "+", 
                              negativeMeasurements$measurementsVars,
                             ">=", -1)
  }
  
  return(c(cOf1, cOf2))
} 