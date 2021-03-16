## The list of all LP constraints with removed failed-to-write constraints that 
## returns NaN
## 
## Enio Gjerga, 2020

concatenateConstraints <- function(constraintsText){

  allConstraints <- paste0("c", seq_len(length(constraintsText)),
                           ":\t", constraintsText, "\t \t")

  collectNan <- grep(pattern = "NaN", x = allConstraints) 
  if(length(collectNan) > 0){
    allConstraints <- allConstraints[-collectNan] 
  }
  
  return(allConstraints)
}
