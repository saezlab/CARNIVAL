##
all_constraints_wLoop <- function(c0=c0, c1=c1, c2=c2, c3=c3, c4=c4, c5=c5, c6=c6, c7=c7, c8=c8, c9=c9){
  
  allConst <- c(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9)
  
  allConstraints <- paste0("c", 1:length(allConst), ":\t", allConst, "\t \t")
  
  return(allConstraints)
  
}
