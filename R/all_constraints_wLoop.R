#'\code{all_constraints_wLoop}
#'
#'@param c0 Constraints number 0
#'@param c1 Constraints number 1
#'@param c2 Constraints number 2
#'@param c3 Constraints number 3
#'@param c4 Constraints number 4
#'@param c5 Constraints number 5
#'@param c6 Constraints number 6
#'@param c7 Constraints number 7
#'@param c8 Constraints number 8
#'@param c9 Constraints number 9
#'
#'@return The list of all LP constraints with removed failed-to-write constraints that returns NaN

all_constraints_wLoop <- function(c0=c0, c1=c1, c2=c2, c3=c3, c4=c4, c5=c5, c6=c6, c7=c7, c8=c8, c9=c9){
  
  allConst <- c(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9)
  
  allConstraints <- paste0("c", 1:length(allConst), ":\t", allConst, "\t \t")
  
  if(length(grep(pattern = "NaN", x = allConstraints)) > 0){
    return(allConstraints[-grep(pattern = "NaN", x = allConstraints)])
  }
  else{
    return(allConstraints)
  }
  
}
