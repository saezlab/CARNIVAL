## This function returns the list of constraints associated with the 'Absolute 
## Difference' variables and which measure the mis-fit between inferred and 
## measured data.
## 
## Enio Gjerga, 2020

createConstraintsMeasurements_v2 <- function(variables, constraintName = "c0") {
  
  positiveMeasurements <- variables$measurementsDf[variables$measurementsDf$value > 0, ]
  negativeMeasurements <- variables$measurementsDf[variables$measurementsDf$value < 0, ]
  cOf1 <- c()
  cOf2 <- c()
  
  if( nrow(positiveMeasurements) > 0 ) {
    cOf1 <- createConstraint(positiveMeasurements$nodesVars, "-", 
                              positiveMeasurements$measurementsVars,
                              "<=", 1)
    cOf2 <- createConstraint(positiveMeasurements$nodesVars, "+", 
                              positiveMeasurements$measurementsVars,
                               ">=", 1)
   
  }
  
  if( nrow(negativeMeasurements) > 0 ) {
    cOf1 <- c( cOf1, createConstraint(negativeMeasurements$nodesVars, "-", 
                              negativeMeasurements$measurementsVars,
                             "<=", -1) )
    cOf2 <- c( cOf2, createConstraint(negativeMeasurements$nodesVars, "+", 
                              negativeMeasurements$measurementsVars,
                             ">=", -1) )
  }
  
  cOf <- list( c(cOf1, cOf2) )
  names(cOf) <- constraintName
  return( cOf )
} 
