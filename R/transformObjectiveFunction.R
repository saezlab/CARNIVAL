#'\code{transformObjectiveFunction}
#'
#' Vectorizing the objective function
#'
#'Enio Gjerga, 2020

transformObjectiveFunction <- function(mt = mt, lpFile = lpFile){
  
  matrixProblem <- matrix(data = , nrow = 1, ncol = nrow(mt))
  idx <- which(grepl(pattern = "Obj:", x = lpFile$`enter Problem`))
  
  problem <- as.character(lpFile[idx, ])
  problem <- gsub(pattern = "Obj:\t ", replacement = "", x = problem, 
                  fixed = TRUE)
  
  coefficients <- as.numeric(
    unlist(
      lapply(
        strsplit(
          x = strsplit(
            x = problem, split = "+ ", fixed = TRUE)[[1]], 
          split = " ", fixed = TRUE), "[[", 1)))
  
  varID <- as.character(
    unlist(
      lapply(
        strsplit(
          x = strsplit(
            x = problem, split = "+ ", fixed = TRUE)[[1]], 
          split = " ", fixed = TRUE), "[[", 2)))
  
  idx <- c()
  for(ii in 1:length(varID)){
    idx = c(idx, which(mt[, 1]==varID[ii]))
  }
  
  f.obj = rep(0, nrow(mt))
  f.obj[idx] = coefficients
  
  return(f.obj)
}